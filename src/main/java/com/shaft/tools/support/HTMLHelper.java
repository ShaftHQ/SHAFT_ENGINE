package com.shaft.tools.support;

public enum HTMLHelper {
    CHECKPOINT_COUNTER("""
            <!DOCTYPE html>
            <html lang="en">

            <head>
                <meta charset="UTF-8">
                <meta name="viewport" content=
                    "width=device-width, initial-scale=1.0">
                    
                <title>Checkpoints Report</title>

                <link rel="stylesheet" type="text/css" href="https://colorlib.com/etc/tb/Table_Fixed_Header/vendor/bootstrap/css/bootstrap.min.css">
                <br>
                <style>
                body {
                  background-color: #e9edf1;
                }
                .content {
                  max-width: 1400px;
                  margin: auto;
                  background-color:white;
                }
                .piechart {
                    display: block;
                    position: relative;
                    width: 220px;
                    height: 220px;
                    border-radius: 50%;
                    background-image: conic-gradient(
                        MediumSeaGreen ${CHECKPOINTS_PASSED_PERCENTAGE}deg,
                        Tomato 0);
                }
                
                body,
                .piechart {
                    display: block;
                    justify-content: center;
                    align-items: center;
                }
                
                .table100 .ps__rail-y{
                    width:9px;
                    background-color:transparent;
                    opacity:1!important;
                    right:5px
                }
                .table100 .ps__rail-y::before{
                    content:"";
                    display: block;
                    position: relative;
                    background-color:#ebebeb;
                    border-radius:5px;
                    width:100%;
                    height:calc(100% - 30px);
                    left:0;
                    top:15px
                }
                .table100 .ps__rail-y .ps__thumb-y{
                    width:100%;
                    right:0;
                    background-color:transparent;
                    opacity:1!important
                }
                .table100 .ps__rail-y .ps__thumb-y::before{
                    content:"";
                    display: block;
                    position: relative;
                    background-color:#ccc;
                    border-radius:5px;
                    width:100%;
                    height:calc(100% - 30px);
                    left:0;
                    top:15px
                }
                .limiter{
                    width:100%;
                    margin:0 auto
                }
                .container-table100{
                    width:100%;
                    min-height:100%;
                    background:#fff;
                    align-items:center;
                    justify-content:center;
                }
                .wrap-table100{
                    width:75%
                }
                .table100{
                    background-color:#fff
                }
                table{
                    width:100%
                }
                th,td{
                    font-weight:unset;
                    padding-right:10px
                }
                .table100-head th{
                    padding-top:18px;
                    padding-bottom:18px
                }
                .table100-body td{
                    padding-top:16px;
                    padding-bottom:16px
                }
                .table100{
                    position: relative;
                    padding-top:60px;
                    padding-bottom:60px
                }
                .table100-head{
                    position: relative;
                    width:100%;
                    top:0;
                    left:0
                }
                .table100-body{
                    max-height:585px;
                    overflow:auto
                }
                .table100.ver5{
                    margin-right:-30px
                }
                .table100.ver5 .table100-head{
                    padding-right:30px
                }
                .table100.ver5 th{
                    font-family: Roboto,sans-serif;
                    font-size:14px;
                    font-weight: bold;
                    color:#555;
                    line-height:1.4;
                    text-transform:uppercase;
                    background-color:transparent
                }
                .table100.ver5 td{
                    font-family: Roboto,sans-serif;
                    font-size:15px;
                    color:gray;
                    line-height:1.4;
                    background-color:#f7f7f7
                }
                .table100.ver5 .table100-body tr{
                    overflow:hidden;
                    border-bottom:10px solid #fff;
                    border-radius:10px
                }
                .table100.ver5 .table100-body table{
                    border-collapse:separate;
                    border-spacing:0 10px
                }
                .table100.ver5 .table100-body td{
                    border:solid 1px transparent;
                    border-style:solid none;
                    padding-top:10px;
                    padding-bottom:10px
                }
                .table100.ver5 .table100-body td:first-child{
                    border-left-style:solid;
                    border-top-left-radius:10px;
                    border-bottom-left-radius:10px
                }
                .table100.ver5 .table100-body td:last-child{
                    border-right-style:solid;
                    border-bottom-right-radius:10px;
                    border-top-right-radius:10px
                }
                .table100.ver5 tr:hover td{
                    background-color:#ebebeb;
                    cursor:pointer
                }
                .table100.ver5 .table100-head th{
                    padding-top:25px;
                    padding-bottom:25px
                }
                .table100.ver5{
                    overflow:hidden
                }
                .table100.ver5 .table100-body{
                    padding-right:30px
                }
                .table100.ver5 .ps__rail-y{
                    right:0
                }
                .table100.ver5 .ps__rail-y::before{
                    background-color:#ebebeb
                }
                .table100.ver5 .ps__rail-y .ps__thumb-y::before{
                    background-color:#ccc
                }
                .column1{
                    width:5%;
                    padding-left:40px
                }
                .column2{
                    width:10%
                }
                .column3{
                    width:75%
                }
                .column4{
                    width:10%
                }
                
            </style>
            </head>
                            
            <body>
            <center>
            <h1>Checkpoints Report</h1>
            <br><div class="content">
            <h2><br><br>Summary</h2>
            <br><div class="piechart"></div>
            <br><h3><b>Total</b>:&nbsp${CHECKPOINTS_TOTAL}&nbsp|&nbsp<font style="color:MediumSeaGreen;"><b>Passed:</b>&nbsp${CHECKPOINTS_PASSED}</font>&nbsp|&nbsp<font style="color:Tomato;"><b>Failed:</b>&nbsp${CHECKPOINTS_FAILED}</font></h3>
            <h2><br><br>Details</h2>
            <div class="limiter">
            <div class="container-table100">
            <div class="wrap-table100">
            <div class="table100 ver5 m-b-110">
            <div class="table100-head">
            <table>
            <thead>
            <tr class="row100 head">
            <th class="cell100 column1">ID</th>
            <th class="cell100 column2">Type</th>
            <th class="cell100 column3">Message</th>
            <th class="cell100 column4">Status</th>
            </tr>
            </thead>
            </table>
            </div>
            <div class="table100-body js-pscroll ps ps--active-y">
            <table>
            <tbody>${CHECKPOINTS_DETAILS}</tbody>
            </table>
            <div class="ps__rail-x" style="left: 0px; bottom: 0px;"><div class="ps__thumb-x" tabindex="0" style="left: 0px; width: 0px;"></div></div></div>
            </div>
            </div>
            </div>
            </div>
            <br><br>
            </div>
            </center>
            </body>
            </html>"""),
    CHECKPOINT_DETAILS_FORMAT("<tr class=\"row100 body\"><td class=\"cell100 column1\">%d</td><td class=\"cell100 column2\">%s</td><td class=\"cell100 column3\">%s</td><td class=\"cell100 column4\">%s</td></tr>");

    private final String value;

    HTMLHelper(String type) {
        this.value = type;
    }

    public String getValue() {
        return value;
    }
}
