// ==UserScript==
// @name        Remove mail side bar
// @namespace   https://mail.google.com/
// @description Remove mail side bar
// @include     https://mail.google.com/mail/*
// @version     1
// @grant       none
// ==/UserScript==

var rules = ".Bu.y3 { display: none !important; }";

var style = document.createElement('style');
style.type = 'text/css';
if (style.styleSheet)
    style.styleSheet.cssText = rules;
else
    style.appendChild(document.createTextNode(rules));
document.getElementsByTagName('head')[0].appendChild(style);
