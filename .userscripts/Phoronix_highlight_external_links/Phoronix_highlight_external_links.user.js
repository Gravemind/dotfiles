// ==UserScript==
// @name     Phoronix highlight external links
// @include  https://www.phoronix.com/*
// @grant    none
// @version  1
// @grant    none
// ==/UserScript==

var style = document.createElement('style');
style.textContent = 'a[href^="http"]:not([href^="https://www.phoronix.com"]) { color : blue !important;}';
document.getElementsByTagName('head')[0].appendChild(style);
