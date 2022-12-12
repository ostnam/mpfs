import sqlite3
import os
from typing import List

from feed import FeedEntry, Feed

class FeedDb:
    """
    Handles the caching of subscribed feeds and feed items.
    """
    def __init__(self):
        if "MPFS_PRODUCTION" in os.environ:
            self.conn = sqlite3.connect("/data/feeds.db", check_same_thread=False)
        else:
            self.conn = sqlite3.connect("./feeds.db", check_same_thread=False)
        self.init_db()

    def init_db(self):
        """
        Initializes the database.
        """

        self.conn.execute("""
            CREATE TABLE IF NOT EXISTS entries (
                title text,
                link text,
                published text,
                seen int,
                feed text,
                CONSTRAINT uniqs UNIQUE (title, link, feed)
            );
        """)

        self.conn.execute("""
            CREATE TABLE IF NOT EXISTS feeds (
                name text NOT NULL,
                url text PRIMARY KEY
            );
        """)

    def get_entries(self,
                    feeds_url: List[Feed],
                    only_unseen: bool=True) -> list[FeedEntry]:
        """
        Gets all the entries of the passed-in feeds.
        """
        query = """SELECT *
                   FROM entries
                   WHERE feed in ({feeds})""".format(
            feeds=','.join(['?']*len(feeds_url))) + \
            "AND seen = 0;" if only_unseen else ";"

        raw = self.conn.execute(query, feeds_url).fetchall()
        return [FeedEntry.from_sql(i) for i in raw]

    def save_entry(self, entry: FeedEntry):
        """
        Write an entry into the database.
        """
        self.conn.execute("""
            INSERT INTO entries
            VALUES (?, ?, ?, ?, ?);
            """, entry.to_sql())
        self.conn.commit()

    def save_entries(self, entries: List[FeedEntry]):
        """
        Write multiple entries in the database.
        """
        self.conn.executemany("""
            INSERT OR IGNORE entries
            VALUES (?, ?, ?, ?, ?);
            """, [e.to_sql() for e in entries])
        self.conn.commit()

    def get_feeds(self) -> List[Feed]:
        """
        Returns all the subscribed feeds.
        """
        raw = self.conn.execute("SELECT * FROM feeds;")
        return [Feed.from_sql(i) for i in raw]

    def add_feed(self, feed: Feed):
        """
        Registers a feed subscription into the database.
        """
        self.conn.execute("""
            INSERT INTO feeds
            VALUES (?, ?);
            """, feed.to_sql())
        self.conn.commit()

    def delete_feed(self, feed: Feed):
        """
        Registers the unsubscription to a feed into the database.
        """
        self.conn.execute("""
            DELETE FROM feeds
            WHERE name = ? and url = ?;
        """, feed.to_sql())
        self.conn.commit()

    def mark_seen(self, link: str):
        """
        Marks an entry as seen.
        """
        self.conn.execute("""
            UPDATE entries
            SET seen = 1
            WHERE link = ?;
            """, (link,))
        self.conn.commit()
