(function add() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    var status = document.createElement("p");
    status.innerHTML = "add offer status: <font color=\"green\">ready</font>";
    div.appendChild(status);
    div.appendChild(br());
    var contract = text_input("channel offer: ", div);
    div.appendChild(br());
    var Button = button_maker2("publish the channel offer", publish);
    div.appendChild(Button);
    function publish() {
        status.innerHTML = "add offer status: <font color=\"red\">failed</font>";
        var c = JSON.parse(contract.value);
        variable_public_get(["add", c], function(X) {
            status.innerHTML = "add offer status: <font color=\"green\">successfully posted a trade</font>";
        });
    };
})();
