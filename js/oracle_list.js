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
            var direction;
            if (h[4] == 2) {
                direction = "true";
            } else {
                direction = "false";
            }
            var text = "price = ".concat(h[3]).concat("; you win if the result is ").concat(direction).concat("; they pay = ").concat(h[7]).concat("; you pay = ").concat(h[8]);
            t.innerHTML = text;
            offers.appendChild(t);
            var button = button_maker2("display this contract", function() { display_contract(h[1]) });
            offers.appendChild(button);
            offers.appendChild(br());
            display_offers(l.slice(1));
        }
    };
    function display_contract(CID) {
        variable_public_get(["get_offer_contract", CID], function(C) {
            contract_div.innerHTML = JSON.stringify(C[1]);
            //console.log(JSON.stringify(C[1]));
        });
    };
})();
