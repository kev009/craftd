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
    ticks = ["Sun 02/06", "Mon 02/07", "Tues 02/08", "Weds 02/09", "Thurs 02/10", "Fri 02/11", "Sat 02/12"];
    
    // Establish options in an object for easier reuse
    playersBar = {
        legend: {
            show: true,
            location: 'nw'
        },
        seriesColors: ["#891e1e", "#336691", "#389947", "#7c70a9", "#c1b758", "#b77e2e", "#488e83"],
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
        seriesColors: ["#891e1e", "#336691", "#389947", "#7c70a9", "#c1b758", "#b77e2e", "#488e83"],
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
    
    playersGraph = {
        legend: {
            show: true,
            location: 'nw'
        },
        seriesColors: ["#891e1e", "#336691", "#389947", "#7c70a9", "#c1b758", "#b77e2e", "#488e83"],
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
    
    // Generate a plot!
    $.jqplot('playersBar', [playersDefault, playersMod, playersVIP, playersAdmin], playersBar);
    $.jqplot('playersStack', [playersDefault, playersMod, playersVIP, playersAdmin], playersStack);
    $.jqplot('playersGraph', [playersDefault, playersMod, playersVIP, playersAdmin], playersGraph);

    // Scroll to bottom of console output
    // TODO: This bombs if there's no "output" element...
    var objDiv = document.getElementById("output");
    objDiv.scrollTop = objDiv.scrollHeight;
    
});
