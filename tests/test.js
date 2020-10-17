// Hashed Highlighting
// http://www.wilfred.me.uk/blog/2014/09/27/the-definitive-guide-to-syntax-highlighting/
$('#preview a').each(function() {
    var $a = $(this),
        url = $a.attr('href'),
        iconUrl;

    if (isExternal(url)) {
        iconUrl = "//getfavicon.appspot.com/" + encodeURIComponent(url);
        $a.after('<img class="favicon" src="' + iconUrl + '">');
    }
});


// Refactoring

function calculate_sum(i) {
    alert('Adding ' + 1 + ' to ' + i);
    return 1 + i;
}

function show_sum() {

    // Here is the function call:

    alert('Result: ' + calculate_sum(5));
}

function Parenizor() {
    return '(' + this.getValue() + ')';
}

var browserName = "N/A";
if (navigator.appName.indexOf("Netscape") != -1) {
    browserName = "NS";
} else if (navigator.appName.indexOf("Microsoft") != -1) {
    browserName = "MSIE";
} else if (navigator.appName.indexOf("Opera") != -1) {
    browserName = "O";
}

function result() {

    // some code here
}

function show_result() {

    // Here is the function call:

    alert('Result: ' + result());
}

// Auto-complete
// See http://stackoverflow.com/questions/239732/things-possible-in-intellij-that-arent-possible-in-eclipse

function Person(name,address) {
    this.getName = function() { return name };
    this.getAddress = function() { return address };
}

Person.prototype.hello = function() {
    // return "I'm " + this.getName() + " from " + this.get<CTRL-SPACE>;
    // return "I'm " + this.getName() + " from " + this.get
}

// and it shows ONLY getName() and getAddress(), no matter how may get* methods
// you have in other JS objects in your project, and ctrl-click on
// this.getName() brings you to where this one is defined, even if there are
// some other getName() functions in your project.

// -- From TernJS.net

// Use ctrl-space to complete something
co
document.body.a

// Put the cursor in or after an expression, press ctrl-i to
// find its type

var foo = ["array", "of", "strings"]
var bar = foo.slice(0, 2).join("").split("a")[0]

// Works for locally defined types too.

function CTor() { this.size = 10 }
CTor.prototype.hallo = "hallo"

var baz = new CTor
baz.

    // You can press ctrl-q when the cursor is on a variable
    // name to rename it. Try it with CTor...

    // When the cursor is in an argument list, the arguments
    // are shown below the editor.

    [1].reduce(  )
