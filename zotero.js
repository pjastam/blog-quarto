'use strict';

for (let i = 2022; i >= 2021; i--) {
    document.write(`<h2 > ${i} </h2>`);
    document.write(`<div id=year${i}>` + `Loading ${i} publications </div>`);

    fetch(`https://api.zotero.org/users/24775/publications/items?format=bib&style=apa&linkwrap=1&itemType=journalArticle || Report&q=${i}`)
        .then(function (response) {
            return response.text();
        })
        .then(function (body) {
            document.getElementById("year" + i).innerHTML = body;
        });
    document.write("<br/>")
}
