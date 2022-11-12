from typing import List

import db
from feed import Feed, FeedEntry, refresh_feed

class DataManager:
    def __init__(self):
        self.db = db.FeedDb()

    def get_subscribed_feeds(self) -> List[Feed]:
        return self.db.get_feeds()

    def get_entries(
        self,
        feeds: List[Feed]) -> list[FeedEntry]:
        entries = [refresh_feed(feed.url) for feed in feeds]
        flat_entries = [entry for entry_list in entries
                              for entry in entry_list]

        return flat_entries
