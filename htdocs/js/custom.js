// Custom JQuery/Javascript

$(document).ready(function() {

    // Various dynamic style tweaks
    $("tbody tr:even").addClass("alt");
    $("#output li:even").addClass("alt");
    $("#outwrap").resizable({ handles: 's' });

    // jqPlot test
    $.jqplot.config.enablePlugins = true;
    
    // Establish player data
    playersDefault = [134, 54, 42, 85, 89, 152, 250];
    playersMod = [25, 21, 18, 11, 42, 51, 68];
    playersVIP = [11, 3, 6, 16, 26, 11, 34];
    playersAdmin = [2, 5, 6, 1, 4, 10, 14];
    
    // Establish ticks
    ticks = ["Sun", "Mon", "Tues", "Weds", "Thurs", "Fri", "Sat"];
    
    // Establish options in an object for easier reuse
    playersBar = {
        legend: {
            show: true,
            location: 'nw'
        },
       series: [
          {label: 'Default'},
          {label: 'Mod'},
          {label: 'VIP'},
          {label: 'Admin'}
        ],
        grid: {
            shadow: true,
            borderWidth: .5
        },
        title: "Players Past Week",
        seriesDefaults: {
            renderer: $.jqplot.BarRenderer,
            pointLabels: {
                location: 's'
            }
        },
       axes: {
            xaxis: {
                renderer: $.jqplot.CategoryAxisRenderer,
                ticks: ticks
            },
            yaxis: {
                max: 300,
                min: 0,
                tickOptions: {
                    formatString: '%d'
                }
            }
        }
    }
    
    playersStack = {
        stackSeries: true,
        legend: {
            show: true,
            location: 'nw'
        },
       series: [
          {label: 'Default'},
          {label: 'Mod'},
          {label: 'VIP'},
          {label: 'Admin'}
        ],
        grid: {
            shadow: true,
            borderWidth: .5
        },
        title: "Players Past Week",
        seriesDefaults: {
            fill: true,
            showMarker: false
        },
       axes: {
            xaxis: {
                renderer: $.jqplot.CategoryAxisRenderer,
                ticks: ticks
            },
            yaxis: {
                max: 400,
                min: 0,
                tickOptions: {
                    formatString: '%d'
                }
            }
        }
    }
    
    // Generate a plot!
    $.jqplot('playersBar', [playersDefault, playersMod, playersVIP, playersAdmin], playersBar);
    $.jqplot('playersStack', [playersDefault, playersMod, playersVIP, playersAdmin], playersStack);

    // Scroll to bottom of console output
    // TODO: This bombs if there's no "output" element...
    var objDiv = document.getElementById("output");
    objDiv.scrollTop = objDiv.scrollHeight;
    
});
