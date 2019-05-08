function br() {
    return document.createElement("br");
};
function button_maker2(val, fun) {
    var button = document.createElement("input");
    button.type = "button";
    button.value = val;
    button.onclick = fun;
    return button;
};
function text(a) {
    var x2 = document.createElement("h8");
    x2.innerHTML = a;
    return x2;
};
function text_input(query, div) {
    var x = document.createElement("INPUT");
    x.type = "text";
    var q = text(query);
    div.appendChild(q);
    div.appendChild(x);
    return x;
};
function token_units() { return 100000000 }; // VEO
function s2c(x) { return x / token_units(); }
function c2s(x) {
    return Math.floor(parseFloat(x.value, 10) * token_units());
}
