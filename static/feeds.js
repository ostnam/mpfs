"use strict";

function feeds_main(entries, feeds) {
    let feed = new Feed(entries, feeds);
}

class FeedEntry {
    constructor(json) {
        this.title = json.title;
        this.link = json.link;
        this.published = new Date(json.published);
        this.seen = json.seen;
        this.feed = json.feed;
    }

    toElement(parent_feed) {
        let root = document.createElement("div");
        root.setAttribute("class", "entry");

        let table = document.createElement("table");
        table.setAttribute("border", "0");
        table.setAttribute("width", "100%");
        table.setAttribute("class", "top_table");

        let tbody = document.createElement("tbody");
        let tr = document.createElement("tr");
        let td1 = document.createElement("td");
        td1.setAttribute("class", "entry_top");


        let a = document.createElement("a");
        a.setAttribute("href", this.link);

        let title = document.createElement("b");
        title.innerHTML = this.title;

        let td2 = document.createElement("td2");
        td2.setAttribute("class", "del_cross");
        td2.innerHTML = "🞫";

        let bottom = document.createElement("div");
        bottom.setAttribute("class", "entry_bottom");
        const formatted_date = this.published.toString().split("(")[0];
        bottom.innerHTML = `${parent_feed}, ${formatted_date}`;

        root.appendChild(table);
        table.appendChild(tbody);
        tbody.appendChild(tr);
        tr.appendChild(td1);
        td1.appendChild(a);
        a.appendChild(title);
        tr.appendChild(td2);
        root.appendChild(bottom);

        return root;
    }
}

class Feed {
    entries = new Array();
    feeds = new Map();
    entriesRoot = undefined;

    constructor(entries, feeds) {
        this.entries = entries.map(entry => new FeedEntry(entry));
        this.entriesRoot = document.getElementById("entries");
        this.leftBar = document.getElementById("leftbar");

        this.feeds = new Map(feeds.map(a => [a[1], a[0]]));

        for (let [feed_url, feed_name] of this.feeds) {
            this.render_feed(feed_url, feed_name);
        }

        for (let entry of this.entries) {
            this.render_entry(entry);
        }
    }

    render_feed(feed_url, feed_name) {
        let row = document.createElement("div");
        row.setAttribute("class", "feed");
        row.innerHTML = feed_name;
        this.leftBar.appendChild(row);
    }

    render_entry(entry) {
        const parent_feed = this.feeds.has(entry.feed) ?
            this.feeds.get(entry.feed) : entry.feed;
        this.entriesRoot.appendChild(entry.toElement(parent_feed));
    }
}
