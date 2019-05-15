(function() {
    var div = document.createElement("div");
    document.body.appendChild(div);

    var title2 = document.createElement("h3");
    title2.innerHTML = "Raw Contract";
    div.appendChild(title2);
    glossary.link2(div, "accepting_channel_offer", "how to accept a trade");
    var contract_div = document.createElement("div");
    div.appendChild(contract_div);

    var title1 = document.createElement("h3");
    title1.innerHTML = "Trades";
    div.appendChild(title1);
    var offers = document.createElement("div");
    div.appendChild(offers);

    var title = document.createElement("h3");
    title.innerHTML = "Oracles, click to see related trades";
    div.appendChild(title);

    variable_public_get(["oracle_list"], function(X) {
        console.log(JSON.stringify(X));
        var l = X.slice(1);
        offers.innerHTML = "";
        display_oracles(l);
    });
    function display_oracles(l) {
        if (JSON.stringify(l) == "[]") {
            return 0;
        } else {
            var h = l[0];
            variable_public_get(["oracle", h[1]], function(Oracle) {
            //variable_public_get(["oracle", h], function(Oracle) {
                console.log(JSON.stringify(Oracle[1]));
                var button = button_maker2(atob(Oracle[1][4]), function() { return display_oracle(Oracle[1][2], Oracle[1][3]) });
                div.appendChild(button);
                div.appendChild(br());
                display_oracles(l.slice(1));
            });
        };
    };
    function display_oracle(Buys, Sells) {
        console.log(JSON.stringify([Buys, Sells]));
        var l = Buys.concat(Sells.slice(1));
        variable_public_get(["get_offers", l], function(l2) {
            console.log(JSON.stringify(l2));
            offers.innerHTML = "";
            return display_offers(l2.slice(1));
        });
    };
    function display_offers(l) {
        if (JSON.stringify(l) == "[]") {
            return 0;
        } else {
            var h = l[0];
            var t = document.createElement("div");
            var type;
            if (h[9] == 1) {
                type = "binary";
                price = h[3];
                return display_offers2(l, h, t, type, price, " or ", "");
            } else if (h[9] == 2) {
                var oid = h[2];
                type = "scalar"
                oracle_limit(oid, function(oracle_max) {
                    console.log("oracle_list callback");
                    console.log(oracle_max);
                    var direction = h[4];
                    if (direction == 2) {
                        price = (1023 - h[3]) * oracle_max / 1023;
                    } else if (direction == 1) {
                        price = h[3] * oracle_max / 1023;
                    } else {
                        console.log("fail");
                        return 0
                    };
                    return display_offers2(l, h, t, type, price, " veo/stablecoin or ", " stablecoin/veo;");
                });
            } else {
                console.log(h[9]);
                console.log("contract type not supported.");
            }
        }
    };
    function display_offers2(l, h, t, type, price, d1message, d2message) {
        var direction;
        if (h[4] == 2) {
            if (type == "binary") {
                direction = "the result is true";
            } else if (type == "scalar") {
                direction = "the price of stablecoin measured in veo increases";
            }
        } else {
            if (type == "binary") {
                direction = "the result is false";
            } else if (type == "scalar") {
                direction = "the price of stablecoin measured in veo decreases";
            }
        }
        var text = "bet type: ".concat(type).concat("; price = ").concat(price.toFixed(5)).concat(d1message).concat((1/price).toFixed(5)).concat(d2message).concat(" you win if ").concat(direction).concat("; they pay = ").concat(s2c(h[7])).concat("; you pay = ").concat(s2c(h[8])).concat("; expires: ").concat(h[5]);
        t.innerHTML = text;
        offers.appendChild(t);
        var button = button_maker2("display this contract", function() { display_contract(h[1]) });
        offers.appendChild(button);
        offers.appendChild(br());
        display_offers(l.slice(1));
    };
    function display_contract(CID) {
        variable_public_get(["get_offer_contract", CID], function(C) {
            contract_div.innerHTML = JSON.stringify(C[1]);
            //console.log(JSON.stringify(C[1]));
        });
    };
})();
