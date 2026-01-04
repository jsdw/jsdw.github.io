// repalce any <email-address> element with a link
// pointed to my email address. jump through hoops to
// try and limit bots grabbing it.
runBehaviour("email-address", function(el){
    setTimeout(function(){
        requestAnimationFrame(function(){
            var link = document.createElement("a");
            link.href = "mailto:" + emailAddress();
            link.appendChild(document.createTextNode(emailAddress()));
            el.replaceWith(link);
        })
    })
});

// makes any element tagged with needs-js visible given
// that JS is running in order to process this.
runBehaviour("[needs-js]", function(el){
    el.attributes.removeNamedItem("needs-js");
});

// if we put maths inside <katex> elements anywhere, we
// lazy load katex and render the math. CSS is in place to
// warn the users that maths can't be displayed if no JS.
runBehaviour("katex", (function(){
    var katexIsLoaded = false;
    var katexIsLoading = false;
    var onload = [];

    function loadKatexAndThenRun(fn){
        if(katexIsLoaded) return fn();
        onload.push(fn);
        if(katexIsLoading) return;
        katexIsLoading = true;

        var css = document.createElement("link");
        css.rel = "stylesheet";
        css.href = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0-alpha2/katex.min.css";
        css.integrity = "sha384-exe4Ak6B0EoJI0ogGxjJ8rn+RN3ftPnEQrGwX59KTCl5ybGzvHGKjhPKk/KC3abb"
        css.crossOrigin = "anonymous";
        document.head.appendChild(css);

        var js = document.createElement("script");
        js.onload = function(){
            katexIsLoaded = true;
            onload.map(function(fn) { fn() });
        };
        js.src = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0-alpha2/katex.min.js";
        js.integrity = "sha384-OMvkZ24ANLwviZR2lVq8ujbE/bUO8IR1FdBrKLQBI14Gq5Xp/lksIccGkmKL8m+h";
        js.crossOrigin = "anonymous";
        document.head.appendChild(js);
    }

    return function(el){
        var maths = (el.textContent || el.innerText || "").trim();
        el.className = "loading";

        loadKatexAndThenRun(function(){
            var out = document.createElement("katex");
            out.className = "rendered";
            try {
                katex.render(maths, out);
            } catch(e) {
                console.warn("Katex failed to render maths: '"+maths+"' - "+String(e));
                el.appendChild(document.createTextNode("Failed to render maths: " + String(e)));
            }
            el.replaceWith(out);
        });
    }

}()))

// create components!
function runBehaviour(selector, fn) {
    var els = document.querySelectorAll(selector);
    for(var i = 0; i < els.length; i++) {
        fn(els[i]);
    }
}

// My email addy, obfuscated.
function emailAddress() {
    return [50,42,55,48,63,13,56,66,52,72,0,64,57].reduce(function(s,n,i){
        return s + String.fromCharCode(n - i + 56);
    },"");
}