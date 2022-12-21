from typing import List
import threading
import concurrent.futures
import time

import db
from feed import Feed, FeedEntry, refresh_feed

class DataManager:
    """
    This class fetches the data from the DB and RSS feeds.
    """
    def __init__(self):
        self.db = db.FeedDb()
        threading.Thread(group=None, target=self.update_loop).start()

    def update_loop(self):
        while True:
            self.update_feeds()
            time.sleep(600)

    def update_feeds(self):
        all_feeds = self.get_subscribed_feeds()
        with concurrent.futures.ThreadPoolExecutor() as p:
            new_entries = [p.submit(refresh_feed, f.url) for f in all_feeds]
            new_entries = [p.result() for p in new_entries]
            threading.Thread(target=self.db.save_entries,
                             args=[sum(new_entries, [])]).start()

    def get_subscribed_feeds(self) -> List[Feed]:
        """
        Returns every subscribed feed.
        """
        return self.db.get_feeds()

    def add_feed(self, feed: Feed) -> None:
        """
        Subscribe to a feed.
        """
        self.db.add_feed(feed)

    def delete_feed(self, feed: Feed) -> None:
        """
        Unsubscribe to a feed.
        """
        self.db.delete_feed(feed)

    def get_entries(self,
                    feeds: List[Feed]) -> list[FeedEntry]:
        """
        Gets all the entries from the requested feeds.
        """
        entries = self.db.get_entries(feeds, True)
        threading.Thread(target=self.update_feeds).start()
        return entries

    def mark_seen(self, url: str) -> None:
        """
        Mark a feed entry as seen.
        """
        self.db.mark_seen(url)
