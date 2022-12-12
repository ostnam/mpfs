from typing import List
import threading

import db
from feed import Feed, FeedEntry, refresh_feed

class DataManager:
    """
    This class fetches the data from the DB and RSS feeds.
    """
    def __init__(self):
        self.db = db.FeedDb()

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
        self.db.delete_feed(feed);

    def get_entries(self,
                    feeds: List[Feed],
                    force_update: bool) -> list[FeedEntry]:
        """
        Gets all the entries from the requested feeds.
        """
        if force_update:
            nested_entries = [refresh_feed(feed.url) for feed in feeds]
            fresh_entries = [entry for entry_list in nested_entries
                             for entry in entry_list]
            db_update_thr = threading.Thread(target=self.db.save_entries,
                                             args=[fresh_entries])
            db_update_thr.run()
            entries = [e for e in fresh_entries if not e.seen]

        else:
            feeds_urls = [f.url for f in feeds]
            entries = self.db.get_entries(feeds_urls, True)

        return entries

    def mark_seen(self, url: str) -> None:
        """
        Mark a feed entry as seen.
        """
        self.db.mark_seen(url)
