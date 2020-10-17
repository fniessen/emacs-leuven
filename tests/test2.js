// http://blog.binchen.org/posts/why-emacs-is-better-editor.html
$(el.completeRegistrationForm).validate({
    ignore: " :hidden",
    rules : {
        password : {
            required : function () { return $(el.password).is(":visible"); }
        },
        accountNumber : {
            required : function () {
                return $(el.accountNumber).is(":visible");
            },
            digits : true
        }
        // ... I skipped next 200 lines which are similar to above lines

    },
    messages : {
        password: {
            required : "Please input a valid password"
        },
        accountNumber: {
            required : "Please provide a valid account number",
            digits : "Please enter only digits",
        }
        // ... I skipped next 200 lines which are similar to above lines
    }
});
