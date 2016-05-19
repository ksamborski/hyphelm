/*
 * hypelm-test_data
 * https://github.com/ksamborski/hyphelm
 *
 * Copyright (c) 2016 Karol Samborski
 * Licensed under the MIT license.
 */

'use strict';

var co = require('co');
var fetch = require('node-fetch');
var cheerio = require('cheerio');

function* wordlist_info(url, word) {
  var res = yield fetch(url);
  var html = yield res.text();
  
  var $ = cheerio.load(html);

  var syllabes = $("div.content > ul.info > li:contains('Typograficzny podział na sylaby: ')");

  if (syllabes.length > 0) {
    return {name: word, syllabes: syllabes.find('b').text().split('-')};
  } else {
    return {};
  }
}

function* wordlist(url) {
  var res = yield fetch(url);
  var html = yield res.text();

  var $ = cheerio.load(html);
  var words = [];

  $("div.content > ul.list > li").each(function(i,el) {
    var $el = $(el);
    words.push({url: $el.find('a').attr("href"), word: $el.text()});
  });

  var next_page = $("span.p_next > a");

  if (next_page) {
    return {next: next_page.attr("href"), words: words};
  } else {
    return {words: words};
  }

}

var letters = [
  'a', 'b', 'c', 'ć', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
  'k', 'l', 'ł', 'm', 'n', 'o', 'ó', 'p', 'r', 's', 'ś',
  't', 'u', 'w', 'y', 'z', 'ź', 'ż'
];

co(function *() {
  var words = {};
  var idx = process.argv[2];
  var letter = letters[idx];
  
  if (!letter) {
    console.log('You have to provide index >= 0 && < 29');
    return;
  }

  var page_words = {next: 'http://wordlist.eu/slowa/na-litere,' + letter + '/'};

  while (!!page_words.next) {
    page_words = yield wordlist(page_words.next);
    for (var w = 0; w < page_words.words.length; w++) {
      var word = page_words.words[w];
      var info = yield wordlist_info(word.url, word.word);

      if (info.name) {
        words[info.name] = info.syllabes;
      }
    }
  }
  
  console.log(JSON.stringify(words));
});
