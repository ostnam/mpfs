import sqlite3
from typing import List

from feed import FeedEntry, Feed

class FeedDb:
    def __init__(self):
        self.conn = sqlite3.connect("feeds.db", check_same_thread=False)
        self.init_db()

    def init_db(self):
        self.conn.execute("""
            CREATE TABLE IF NOT EXISTS entries (
                title text,
                link text,
                published text,
                seen int,
                feed text
            );
        """)

        self.conn.execute("""
            CREATE TABLE IF NOT EXISTS feeds (
                name text NOT NULL,
                url text NOT NULL,
                last_update text
            );
        """)

    def get_entries(
        self,
        feeds: List[str],
        only_unseen: bool=False) -> list[FeedEntry]:
        query = """SELECT *
                   FROM entries
                   WHERE feed in ({feeds})""".format(
            feeds=','.join(['?']*len(feeds))) + \
            "AND seen = 0;" if only_unseen else ";"

        raw = self.conn.execute(query).fetchall()
        return [FeedEntry.from_sql(i) for i in raw]

    def save_entry(self, entry: FeedEntry):
        self.conn.execute("""
            INSERT INTO entries
            VALUES (?, ?, ?, ?, ?);
        """, entry.to_sql())


    def get_feeds(self) -> List[Feed]:
        raw = self.conn.execute("SELECT * FROM feeds;")
        return [Feed.from_sql(i) for i in raw]

    def save_feed(self, feed: Feed):
        self.conn.execute("""
            INSERT INTO feeds
            VALUES (?, ?, ?);
        """, feed.to_sql())
