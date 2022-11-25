"use strict";

class FeedEntry {
    constructor(json) {
        this.title = json.title;
        this.link = json.link;
        this.published = Date.parse(json.published);
        this.seen = json.seen;
        this.feed = json.feed;
    }
}

class Feed {
    entries = new Array();
}

console.log("hello helka!");
