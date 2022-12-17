"use strict";
/**
 * This module is included in /feeds.html, and handles the dynamic scripting of that page.
 */

 // Main function of the feeds.html view.
function feeds_main(entries, feeds) {
    let feed = new FeedPage(entries, feeds);
    build_options(feed);
}

function build_options(feed) {
    let add_feed_button = document.getElementById("add_feed_button");
    add_feed_button.onclick = () => feed.add_feed_popup();

    let delete_feed_btn = document.getElementById("delete_feed_button");
    delete_feed_btn.onclick = () => feed.toggle_delete_feed();

    let settings_btn = document.getElementById("settings_button");
    settings_btn.onclick = () => feed.settings_popup();
}

function close_feed_popup() {
    document.getElementById("feed_popup").remove();
}

function close_settings_popup() {
    document.getElementById("settings_popup").remove();
}

class SubscribedFeed {
    constructor(name, url) {
        this.name = name;
        this.url = url;
    }

    /** Renders a SubscribedFeed in the left bar. */
    render_in_leftbar(left_bar_node, num_elements) {
        let row = document.createElement("div");
        row.setAttribute("class", "feed");

        let feed_name = document.createElement("div");
        feed_name.setAttribute("class", "feed_name");
        feed_name.innerHTML = this.name;

        let counter = document.createElement("div");
        counter.setAttribute("class", "feed_counter");
        counter.innerHTML = num_elements !== 0 ? num_elements.toString() : "";

        left_bar_node.appendChild(row);
        row.appendChild(counter);
        row.appendChild(feed_name);
        this.node = row;
    }

    /** Removes the SubscribedFeed from the left bar.
    */
    destroy() {
        this.node.remove();
    }

    /** Called after clicking on the delete_feed button.
     *  Alters the HTML element in consequence.
     */
    make_deletable(delete_fn) {
        this.node.classList.add("deletable_feed");
        this.node.onclick = () => {
            this.destroy();
            delete_fn(this.name, this.url);
        };
    }

    /** Called after clicking on the delete_feed button,
     *  while already in delete_feed mode.
     *  Returns the HTML element to normal.
     */
    unmake_deletable() {
        this.node.classList.remove("deletable_feed");
        this.node.removeAttribute("onclick");
    }

    fetch_entries() {
        return fetch("/subscriptions", {
                method: "GET",
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify({
                    name: feed.name,
                    url: feed.url
        })}).then(
            res => res.json()
        ).then(
            JSON.parse(json)
        ).then(
            e => e.map(x => new FeedEntry(x))
        );
    }
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
class FeedPage {
    /** Every items of every feed.
     * keys: feed url, values: FeedEntry
     */
    entries = new Map();

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
        this.feeds = new Map(feeds.map(
            a => [a[1], new SubscribedFeed(a[0], a[1])]
        ));

        for (let [_, feed] of this.feeds) {
            this.entries.set(feed.url, new Array());
        }

        for (let entry of entries.map(entry => new FeedEntry(entry))) {
            this.entries.get(entry.feed).push(entry);
        }

        this.entriesRoot = document.getElementById("entries");
        this.leftBar = document.getElementById("leftbar");

        this.delete_feed_ison = false;


        for (let [_, feed] of this.feeds) {
            feed.render_in_leftbar(this.leftBar, this.entries.get(feed.url).length);
        }

        this.render_entries();
    }

    /** Renders every entries.
     */
    render_entries() {
        if (this.current_selected_feed === undefined) {
            let all_entries = new Array();
            for (let [_, entries] of this.entries) {
                all_entries = all_entries.concat(entries);
            }
            all_entries.sort((a, b) => a.published - b.published);
            let new_html_elements = all_entries.map(x => this.render_entry(x));
            document.getElementById("entries").replaceChildren(...new_html_elements);
        }
    }

    /** Called after clicking on the delete_feed button.
     */
    toggle_delete_feed() {
        if (this.delete_feed_ison) {
            for (let [_, feed] of this.feeds) {
                feed.unmake_deletable();
            };
            this.delete_feed_ison = false;
        } else {
            for (let [_, feed] of this.feeds) {
                feed.make_deletable((a, b) => this.drop_feed(a, b));
            };
            this.delete_feed_ison = true;
        }
    };

    /** Remove a feed from the feeds list.
     */
    drop_feed(feed_name, url) {
        this.feeds.delete(url);
        fetch("/subscriptions", {
            method: "DELETE",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                name: feed_name,
                url: url})
        });
    };

    /** Renders an entry. */
    render_entry(entry) {
        const parent_feed = this.feeds.has(entry.feed) ?
            this.feeds.get(entry.feed).name : entry.feed;
        return entry.toElement(parent_feed);
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

        let title = document.createElement("b");
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

    /** Draws the popup window that appears when clicking on the settings button. */
    settings_popup() {
        let root = document.createElement("div");
        root.setAttribute("class", "modal");
        root.setAttribute("id", "settings_popup");
        root.style.display = "block";

        let content = document.createElement("div");
        content.setAttribute("class", "modal-content");
        root.appendChild(content);

        let top_bar = document.createElement("div");
        top_bar.setAttribute("class", "settings_top_bar");
        content.appendChild(top_bar);

        let title = document.createElement("b");
        title.innerHTML = "Settings";
        title.setAttribute("id", "settings_title")
        top_bar.appendChild(title);

        let close_btn = document.createElement("div");
        close_btn.setAttribute("onclick", "close_settings_popup()");
        close_btn.setAttribute("class", "option_button");
        top_bar.appendChild(close_btn);

        let close_img = document.createElement("img");
        close_img.setAttribute("class", "option_img");
        close_img.setAttribute("src", "static/images/close.png");
        close_btn.appendChild(close_img)

        let input_json_container = document.createElement("div");
        input_json_container.setAttribute("class", "settings_row");
        content.appendChild(input_json_container);

        let input_json_feeds = document.createElement("input");
        input_json_feeds.type = "file";
        input_json_feeds.style.display = "none";
        input_json_container.appendChild(input_json_feeds);
        input_json_feeds.onchange = e => {
            let file = e.target.files[0];
            let reader = new FileReader();
            reader.readAsText(file, "utf-8");
            reader.onload = readerEvent => {
                let content = readerEvent.target.result;
                let parsed = JSON.parse(content).map(
                    x => new SubscribedFeed(x.name, x.url)
                );
                parsed.forEach((feed) => this.register_feed(feed));
            }
        };

        let input_json_text = document.createElement("button");
        input_json_text.innerHTML = "Load feeds from JSON.";
        input_json_text.onclick = () => input_json_feeds.click();
        input_json_container.appendChild(input_json_text);

        document.body.appendChild(root);
    }

    /** Adds a feed, after filling the form and submitting it. */
    add_feed(event) {
        document.getElementById('feed_submit').disabled = true;
        const url = document.getElementById("feed_url_input").value;
        const name = document.getElementById("feed_name_input").value;
        let new_feed = new SubscribedFeed(name, url);
        this.register_feed(new_feed, true);
    }

    register_feed(feed, get_entries) {
        for (let prev_feed of this.feeds) {
            if (prev_feed.url === feed.url) {
                return;
            }
        }
        fetch("/subscriptions", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                name: feed.name,
                url: feed.url})
        });
        this.feeds.set(feed.url, feed);
        if (get_entries) {
            let entries = feed.fetch_entries();
            this.entries.set(feed.url, entries);
        }
        feed.render_in_leftbar(
            this.leftBar,
            this.entries.get(feed.url).length
        );
    }
}
