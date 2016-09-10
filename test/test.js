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
