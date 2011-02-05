// Custom JQuery/Javascript

$(document).ready(function() {

    $("tbody tr:even").addClass("alt");
    $("#output li:even").addClass("alt");
    $("#outwrap").resizable({ handles: 's' });

    // Scroll to bottom of console output
    var objDiv = document.getElementById("output");
    objDiv.scrollTop = objDiv.scrollHeight;

});
