{-# LANGUAGE QuasiQuotes #-}
module Javascript where

import Language.Javascript.JMacro
import Language.Javascript.JMacro.Prelude

paintGraph btc usd = do let btc = [65,59,90,81,56,55,40] :: [Int]
                        let usd = [28,48,40,19,96,27,100] :: [Int]
                            in [jmacro|
    var lineChartData = {
        labels : ["January","February","March","April","May","June","July"],
        datasets : [
            {
                    fillColor : "rgba(220,220,220,0.5)",
                    strokeColor : "rgba(220,220,220,1)",
                    pointColor : "rgba(220,220,220,1)",
                    pointStrokeColor : "#fff",
                    data : `(btc)`
            },
            {
                    fillColor : "rgba(151,187,205,0.5)",
                    strokeColor : "rgba(151,187,205,1)",
                    pointColor : "rgba(151,187,205,1)",
                    pointStrokeColor : "#fff",
                    data : `(usd)`
            }
        ]
    };

    var myLine = new Chart(document.getElementById("canvas").getContext("2d")).Line(lineChartData);
|]
