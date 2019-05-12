(function() {
    var div = document.createElement("div");
    document.body.appendChild(div);

    var title2 = document.createElement("h3");
    title2.innerHTML = "Raw Contract";
    div.appendChild(title2);
    var contract_div = document.createElement("div");
    div.appendChild(contract_div);

    var title1 = document.createElement("h3");
    title1.innerHTML = "Trades";
    div.appendChild(title1);
    var offers = document.createElement("div");
    div.appendChild(offers);

    var title = document.createElement("h3");
    title.innerHTML = "Oracles";
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
            var direction, type;
            if (h[9] == 1) {
                type = "binary";
                price = h[3];
                return display_offers2(l, h, t, direction, type, price, " or ", "");
            } else if (h[9] == 2) {
                var oid = h[2];
                type = "scalar"
                oracle_limit(oid, function(oracle_max) {
                    console.log("oracle_list callback");
                    console.log(oracle_max);
                    price = (1023 - h[3]) * oracle_max / 1023;
                    return display_offers2(l, h, t, direction, type, price, " stablecoin/veo or ", " veo/stablecoin;");
                });
            } else {
                console.log(h[9]);
                console.log("contract type not supported.");
            }
        }
    };
    function display_offers2(l, h, t, direction, type, price, d1message, d2message) {
        if (h[4] == 2) {
            direction = "true/long/stablecoin";
        } else {
            direction = "false/short/long-veo";
        }
        var text = "bet type: ".concat(type).concat("; price = ").concat(price).concat(d1message).concat(1/price).concat(d2message).concat(" you win if the result is ").concat(direction).concat("; they pay = ").concat(s2c(h[7])).concat("; you pay = ").concat(s2c(h[8])).concat("; expires: ").concat(h[5]);
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
