"use strict";
/**
 * This module is included in /feeds.html, and handles the dynamic scripting of that page.
 */

 // Main function of the feeds.html view.
function feeds_main(entries, feeds) {
    let feed = new Feed(entries, feeds);
    build_options(feed);
}

function build_options(feed) {
    let add_feed_button = document.getElementById("add_feed_button");
    add_feed_button.onclick = () => feed.add_feed_popup();
}

/** Represents a single item in the RSS feed.
 */
class FeedEntry {
    constructor(json) {
        this.title = json.title;
        this.link = json.link;
        this.published = new Date(json.published);
        this.seen = json.seen;
        this.feed = json.feed;
    }

    /** Produces an Element representing the entry.
     * @param {Array[string]} parent_feed: The object representing the RSS feed that entry is part of.
    */
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
        a.onclick = () => {
            this.mark_seen(true);
        };
        a.onauxclick = () => {
            this.mark_seen(true);
        };

        let title = document.createElement("b");
        title.innerHTML = this.title;

        let td2 = document.createElement("td");
        td2.setAttribute("class", "del_cross");
        td2.innerHTML = "🞫";
        td2.onclick = () => {
            this.mark_seen();
            this.delete_element();
        };

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

        this.root = root;
        return root;
    }

    /** Marks the entry as seen.
     */
    mark_seen(change_css) {
        if (change_css) {
            this.root.classList.add("seen_entry");
        }
        fetch("/seen", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                url: this.link
            }),
        });
    }

    /** Remove the HTML element from the DOM.
     */
    delete_element() {
        this.root.remove();
    }
}

/** Represents the entire feed view.
 */
class Feed {
    /** Every items of every feed. */
    entries = new Array();

    /** Every feed. */
    feeds = new Map();

    /** The root Element, of which every feed displayed entry is a child. */
    entriesRoot = undefined;

    /** The leftBar container element */
    leftBar = undefined;

    /**
     * @param {string} entries: JSON string representing a FeedEntry.
     * @param {Array[string]} feeds
     */
    constructor(entries, feeds) {
        this.entries = entries.map(entry => new FeedEntry(entry));
        this.entriesRoot = document.getElementById("entries");
        this.leftBar = document.getElementById("leftbar");

        this.feeds = new Map(feeds.map(a => [a[1], a[0]]));

        for (let [feed_url, feed_name] of this.feeds) {
            this.render_feed(feed_name, feed_url);
        }

        for (let entry of this.entries) {
            this.render_entry(entry);
        }
    }

    /** Renders a Feed in the left bar. */
    render_feed(feed_name, feed_url) {
        let row = document.createElement("div");
        row.setAttribute("class", "feed");
        row.innerHTML = feed_name;
        this.leftBar.appendChild(row);
    }

    /** Renders an entry. */
    render_entry(entry) {
        const parent_feed = this.feeds.has(entry.feed) ?
            this.feeds.get(entry.feed) : entry.feed;
        this.entriesRoot.appendChild(entry.toElement(parent_feed));
    }

    /** Draws the popup window that appears when clicking on the button to add a feed. */
    add_feed_popup() {
        let root = document.createElement("div");
        root.setAttribute("class", "modal");
        root.setAttribute("id", "feed_popup");
        root.style.display = "block";

        let content = document.createElement("div");
        content.setAttribute("class", "modal-content");
        root.appendChild(content);

        let top_bar = document.createElement("div");
        top_bar.setAttribute("class", "add_feed_top_bar");
        content.appendChild(top_bar);

        let title = document.createElement("div");
        title.innerHTML = "Add a feed";
        title.setAttribute("id", "add_feed_title")
        top_bar.appendChild(title);

        let close_btn = document.createElement("div");
        close_btn.setAttribute("onclick", "close_feed_popup()");
        close_btn.setAttribute("class", "option_button");
        top_bar.appendChild(close_btn);

        let close_img = document.createElement("img");
        close_img.setAttribute("class", "option_img");
        close_img.setAttribute("src", "static/images/close.png");
        close_btn.appendChild(close_img)

        let main = document.createElement("div");
        content.appendChild(main);

        let form = document.createElement("form");
        form.onsubmit = (event) => {
            event.preventDefault();
            this.add_feed(event);
        };
        main.appendChild(form);

        let feed_name_label = document.createElement("label");
        feed_name_label.setAttribute("for", "feed_name_input");
        feed_name_label.innerHTML = "Feed name";
        let feed_name_input = document.createElement("input");
        feed_name_input.setAttribute("name", "feed_name_input");
        feed_name_input.setAttribute("id", "feed_name_input");
        let feed_url_label = document.createElement("label");
        feed_url_label.setAttribute("for", "feed_url_input");
        feed_url_label.innerHTML = "Feed URL";
        let feed_url_input = document.createElement("input");
        feed_url_input.setAttribute("name", "feed_url_input");
        feed_url_input.setAttribute("id", "feed_url_input");
        let feed_submit = document.createElement("input");
        feed_submit.setAttribute("type", "submit");
        feed_submit.setAttribute("id", "feed_submit");
        feed_submit.setAttribute("value", "Submit");
        let br1 = document.createElement("br");
        let br2 = document.createElement("br");

        form.appendChild(feed_name_label);
        form.appendChild(feed_name_input);
        form.appendChild(br1);
        form.appendChild(feed_url_label);
        form.appendChild(feed_url_input);
        form.appendChild(br2);
        form.appendChild(feed_submit);

        document.body.appendChild(root);
    }

    /** Adds a feed, after filling the form and submitting it. */
    add_feed(event) {
        document.getElementById('feed_submit').disabled = true;
        const url = document.getElementById("feed_url_input").value;
        const name = document.getElementById("feed_name_input").value;

        fetch("/subscriptions", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                name: name,
                url: url})
        });

        this.feeds.set(name, url);
        this.render_feed(name, url);
    }
}

function close_feed_popup() {
    document.getElementById("feed_popup").remove();
}
