// ==UserScript==
// @name        Rename UE
// @namespace   https://docs.unrealengine.com/
// @include     https://docs.unrealengine.com/*
// @version     1
// @grant       none
// ==/UserScript==
if (document.title.substring(0, 16) == 'Unreal Engine | ') {
  document.title = '' + document.title.substring(16);
} 
else if (document.title == 'Search') {
  match_data = document.URL.match(/^https:\/\/docs.unrealengine.com\/.*\/Search\/index.html\?q=(.*)&filter=.*/);
  if (match_data != null) {
    document.title = "Search " + decodeURIComponent((match_data[1] + '').replace(/\+/g, '%20'));
  } else {
    document.title = "Search"
  }
}
