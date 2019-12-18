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
            console.log(JSON.stringify(h));
            variable_public_get(["oracle", h[1]], function(Oracle) {
            //variable_public_get(["oracle", h], function(Oracle) {
                console.log(JSON.stringify(Oracle));
                var t = text(atob(Oracle[1][4]));
                var button = button_maker2("display trades", function() { return display_oracle(Oracle[1][2], Oracle[1][3]) });
                div.appendChild(button);
                div.appendChild(t);
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
        var button = button_maker2("display a copy/pasteable version of this contract", function() { display_contract(h, type) });
        offers.appendChild(button);
        offers.appendChild(br());
        display_offers(l.slice(1));
    };
    function plus_encode(s) {
        if (s == "") { return ""; }
        var h = s[0];
        if (h == "+") { h = "%2B"; }
        return h.concat(plus_encode(s.slice(1)));
    };
    function display_contract(h, type) {
        var CID = h[1];
        variable_public_get(["get_offer_contract", CID], function(C) {
            var copy_contract_link = document.createElement("div");
            var contract_type = type;
            console.log(JSON.stringify(h));
            console.log(JSON.stringify(C));
            var oid = plus_encode(h[2]);
            var UL = C[1][1][18];
            var LL = C[1][1][19];
            F = function(X){
                var Y = parseInt(X) / 100000000;
                return(Y.toFixed(8));
            };
            //var direction = C[1][2][1][2];//either 1 or 2.
            var direction = C[1][1][1];//either 1 or 2.
            var d_string;
            if (direction == 1) {
                d_string = "false";
            } else if (direction == 2) {
                d_string = "true";
            } else {
                console.log(JSON.stringify(C[1][2][1]));
                console.log(direction);
                console.log("badly formed contract offer");
                return(0);
            }
            contract_div.innerHTML = JSON.stringify(C[1]);
            var our_amount = F(C[1][2][1][5]);
            var their_amount = F(C[1][2][1][4]);
            var oracle_height = C[1][3][2];
            copy_contract_link.innerHTML = "<a href=".concat("\"/otc_derivatives.html?auto_fill=").concat(contract_type).concat("&oracle=").concat(oid).concat("&our_amount=").concat(our_amount).concat("&their_amount=").concat(their_amount).concat("&oracle_height=").concat(oracle_height).concat("&bet_direction=").concat(d_string).concat("&upper_limit=").concat(UL).concat("&lower_limit=").concat(LL).concat("\" onclick=\"javascript:event.target.port=8080\"").concat(">open this contract in the contract-editor</a>");
            contract_div.appendChild(copy_contract_link);
            contract_div.appendChild(br());
            //console.log(JSON.stringify(C[1]));
        });
    };
})();
