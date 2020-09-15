/*backtest
start: 2019-08-19 09:00:00
end: 2019-08-23 15:00:00
period: 1m
exchanges: [{"eid":"Futures_CTP","currency":"FUTURES","balance":20000,"minfee":0,"fee":[0,0]}]
mode: 1
*/

/*The principle is to track the buying price and the selling price of the market.
 * Then, according to the price of the market, plus or minus the micro-price increase of the tracking price,
 * This is a market-making strategy. Its business model and logic are to conduct bilateral transactions
 * on exchange-listed limit orders to provide liquidity.
 * The main income of this strategy is the commission fee return provided by the exchange,
 * as well as the price difference earned by buying low and selling high.
*/

enum State {
    STATE_NA,
    STATE_IDLE,
    STATE_HOLD_LONG,
    STATE_HOLD_SHORT,
};

typedef struct {
    double bidPrice;
    double bidAmount;
    double askPrice;
    double askAmount;
} Book;

class HFT {
public:
    HFT() {
        _tradingDay = getTradingWeekDay();
        Log("current trading weekday", _tradingDay);
    }
    int getTradingWeekDay() {
        int seconds = Unix() + 28800;
        int hour = (seconds/3600)%24;
        int weekDay = (seconds/(60*60*24)+4)%7;
        if (hour > 20) {
            weekDay += 1;
        }
        return weekDay;
    }
    State getState() {
        auto orders = exchange.GetOrders();
        if (!orders.Valid || orders.size() == 2) {
            return STATE_NA;
        }

        bool foundCover = false;
        for (auto &order : orders) {
            if (order.Id == _coverId) {
                if ((order.Type == ORDER_TYPE_BUY && order.Price < _book.bidPrice - _toleratePrice) ||
                    (order.Type == ORDER_TYPE_SELL && order.Price > _book.askPrice + _toleratePrice)) {
                    exchange.CancelOrder(order.Id, "Cancel Cover Order");
                    _countCancel++;
                    _countRetry++;
                } else {
                    foundCover = true;
                }
            } else {
                exchange.CancelOrder(order.Id);
                _countCancel++;
            }
        }

        if (foundCover) {
            return STATE_NA;
        }

        auto positions = exchange.GetPosition();
        if (!positions.Valid) {
            return STATE_NA;
        }

        for (auto &pos : positions) {
            if (pos.ContractType == Symbol) {
                _holdPrice = pos.Price;
                _holdAmount = pos.Amount;
                _holdType = pos.Type;
                return pos.Type == PD_LONG || pos.Type == PD_LONG_YD ? STATE_HOLD_LONG : STATE_HOLD_SHORT;
            }
        }
        return STATE_IDLE;
    }

    void stop() {
        Log(exchange.GetOrders());
        Log(exchange.GetPosition());
        Log("Stop");
    }

    bool Loop() {
        if (exchange.IO("status") == 0) {
            LogStatus(_D(), "Server not connect ....");
            Sleep(1000);
            return true;
        }
        if (_initBalance == 0) {
            _initBalance = _C(exchange.GetAccount).Balance;
        }
        auto day = getTradingWeekDay();
        if (day != _tradingDay) {
            _tradingDay = day;
            _countCancel = 0;
        }

        if (_ct.is_null()) {
            Log(_D(), "subscribe", Symbol);
            _ct = exchange.SetContractType(Symbol);
            if (!_ct.is_null()) {
                auto obj = _ct["Commodity"]["CommodityTickSize"];
                int volumeMultiple = 1;
                if (obj.is_null()) { // CTP
                    obj = _ct["PriceTick"];
                    volumeMultiple = _ct["VolumeMultiple"];
                    _exchangeId = _ct["ExchangeID"];
                } else { // Esunny
                    volumeMultiple = _ct["Commodity"]["ContractSize"];
                    _exchangeId = _ct["Commodity"]["ExchangeNo"];
                }
                if (obj.is_null() || obj <= 0) {
                    Panic("PriceTick not found");
                }
                if (_priceTick < 1) {
                    exchange.SetPrecision(1, 0);
                }
                _priceTick = double(obj);
                _toleratePrice = _priceTick * TolerateTick;
                _ins = _ct["InstrumentID"];

                Log(_ins, _exchangeId, "PriceTick:", _priceTick, "VolumeMultiple:", volumeMultiple);
            }
            Sleep(1000);
            return true;
        }
        // Check orders and positions to set state
        auto depth = exchange.GetDepth();
        if (!depth.Valid) {
            LogStatus(_D(), "Market not ready");
            Sleep(1000);
            return true;
        }
        _countTick++;
        _preBook = _book;
        _book.bidPrice = depth.Bids[0].Price;
        _book.bidAmount = depth.Bids[0].Amount;
        _book.askPrice = depth.Asks[0].Price;
        _book.askAmount = depth.Asks[0].Amount;
        // _book not init
        if (_preBook.bidAmount == 0) {
            return true;
        }
        auto st = getState();

        LogStatus(_D(), _ins, "State:", st,
                  "Ask:", depth.Asks[0].Price, depth.Asks[0].Amount,
                  "Bid:", depth.Bids[0].Price, depth.Bids[0].Amount,
                  "Cancel:", _countCancel,
                  "Tick:", _countTick);

        bool forceCover = _countRetry >= _retryMax;
        if (st == STATE_IDLE) {
            if (_holdAmount > 0) {
                if (_countRetry > 0) {
                    _countLoss++;
                } else {
                    _countWin++;
                }
                auto account = exchange.GetAccount();
                if (account.Valid) {
                    LogProfit(_N(account.Balance+account.FrozenBalance-_initBalance, 2), "Win:", _countWin, "Loss:", _countLoss);
                }
            }
            _countRetry = 0;
            _holdAmount = 0;

            if (_countCancel > _cancelMax) {
                Log("Cancel Exceed", _countCancel);
                return false;
            }

            bool canDo = false;
            if (abs(_book.bidPrice - _book.askPrice) > _priceTick * 1) {
                canDo = true;
            }
            if (!canDo) {
                return true;
            }


            auto bidPrice = depth.Bids[0].Price;
            auto askPrice = depth.Asks[0].Price;
            auto bidAmount = 1.0;
            auto askAmount = 1.0;

            if (_preBook.bidPrice > _book.bidPrice && _book.askAmount < _book.bidAmount) {
                bidPrice += _priceTick;
                bidAmount = 2;
            } else if (_preBook.askPrice < _book.askPrice && _book.bidAmount < _book.askAmount) {
                askPrice -= _priceTick;
                askAmount = 2;
            } else {
                return true;
            }
            Log(_book.bidPrice, _book.bidAmount, _book.askPrice, _book.askAmount);
            exchange.SetDirection("buy");
            exchange.Buy(bidPrice, bidAmount);
            exchange.SetDirection("sell");
            exchange.Sell(askPrice, askAmount);
        } else if (st == STATE_HOLD_LONG) {
            exchange.SetDirection((_holdType == PD_LONG && _exchangeId == "SHFE") ? "closebuy_today" : "closebuy");
            auto sellPrice = depth.Asks[0].Price;
            if (sellPrice > _holdPrice) {
                Log(_holdPrice, "Hit #ff0000");
                sellPrice = _holdPrice + ProfitTick;
            } else if (sellPrice < _holdPrice) {
                forceCover = true;
            }
            if (forceCover) {
                Log("StopLoss");
            }
            _coverId = exchange.Sell(forceCover ? depth.Bids[0].Price : sellPrice, _holdAmount);
            if (!_coverId.Valid) {
                return false;
            }
        } else if (st == STATE_HOLD_SHORT) {
            exchange.SetDirection((_holdType == PD_SHORT && _exchangeId == "SHFE") ? "closesell_today" : "closesell");
            auto buyPrice = depth.Bids[0].Price;
            if (buyPrice < _holdPrice) {
                Log(_holdPrice, "Hit #ff0000");
                buyPrice = _holdPrice - ProfitTick;
            } else if (buyPrice > _holdPrice) {
                forceCover = true;
            }
            if (forceCover) {
                Log("StopLoss");
            }
            _coverId = exchange.Buy(forceCover ? depth.Asks[0].Price : buyPrice, _holdAmount);
            if (!_coverId.Valid) {
                return false;
            }
        }
        return true;
    }
private:
    double _holdPrice = 0;
    double _holdAmount = 0;
    int _holdType;
    int _countTick = 0;
    int _countRetry = 0;
    int _countCancel = 0;
    int _period = 20;
    int _tradingDay = 0;
    double _initBalance;
    const int _retryMax = RetryMax;
    const int _cancelMax = 300;

    int _countLoss = 0;
    int _countWin = 0;

    json _ct;
    string _ins;
    string _exchangeId;
    double _priceTick;
    double _toleratePrice;
    Book _book;
    Book _preBook;
    TId _coverId;
};


void main() {
    LogReset();
    SetErrorFilter("ready|timeout");
    Log("Init OK");
    HFT hft;
    while (hft.Loop());
    Log("Exit");
}

