document.addEventListener("DOMContentLoaded", function () {
    // Get the <ul> element by its ID
    var recordList = document.getElementById("recordList");

    // Show a loading message while fetching and parsing data
    recordList.innerHTML = '<li>Loading data, please wait...</li>';

    // Define your JSON data URL
    const url = 'https://api.zotero.org/users/24775/publications/items?start=0&limit=9&format=json&itemType=journalArticle || Report&sort=date&direction=desc';

    fetch(url)
        .then(response => response.json()) // Parse the response as JSON
        .then(jsonData => {
            // Clear the loading message
            recordList.innerHTML = '';

            // Iterate through the JSON data
            jsonData.forEach(function (record) {
                // Select the relevant elements from each record
                var creators = record.data.creators;
                var date = record.data.date;
                var title = record.data.title;
                var publicationTitle = record.data.publicationTitle;
                var url = record.data.url;
                var institution = record.data.institution;
                var extra = record.data.extra;

                // Extract the year from the date
                var year = new Date(date).getFullYear();

                // Create a list item element and set its innerHTML
                var listItem = document.createElement("li");
                listItem.innerHTML = `
                    ${creators.map(creator => `${creator.lastName}`).join(", ")} (${year}). ${url ? `<a href=\"${url}\" target=\"_blank\"><i>${title}</i></a>` : `${title}`}${publicationTitle ? `. ${publicationTitle}` : ''}${institution ? `. ${institution}` : ''}${extra ? `. ${extra}` : ''}.
                `;
                // Append the list item to the <ul> element
                recordList.appendChild(listItem);
            });
        })
        .catch(error => {
            console.error('Error fetching data:', error);
            recordList.innerHTML = '<li>Error fetching data. Please try again later.</li>';
        });
});
