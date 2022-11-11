from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Tuple
import datetime
import time

import feedparser

@dataclass
class FeedEntry:
    title: str
    link: Optional[str]
    published: datetime.datetime
    seen: bool
    feed: str

    @staticmethod
    def from_dict(
        d: feedparser.util.FeedParserDict,
        feed: str) -> Optional[FeedEntry]:

        if not (title := d.get("title")):
            return None
        link = d.get("link")
        published_raw: time.struct_time = d.get("published_parsed")
        if not published_raw:
            return None
        published = datetime.datetime(
                year=published_raw.tm_year,
                month=published_raw.tm_mon,
                day=published_raw.tm_mday,
                hour=published_raw.tm_hour,
                minute=published_raw.tm_min
        )

        return FeedEntry(
            title=title,
            link=link,
            published=published,
            seen=False,
            feed=feed
        )

    @staticmethod
    def from_sql(t: Tuple[str, str, str, int, str]) -> FeedEntry:
        return FeedEntry(
            title=t[0],
            link= l if (l := t[1]) else None,
            published=datetime.datetime.fromisoformat(t[2]),
            seen=bool(t[3]),
            feed=t[4]
        )

    def to_sql(self) -> Tuple[str, str, str, int, str]:
        return (self.title,
                self.link if self.link else "",
                self.published.isoformat(),
                int(self.seen),
                self.feed)

def refresh_feed(feed: str) -> list[FeedEntry]:
    return [entry
            for i in feedparser.parse(feed).entries
            if (entry := FeedEntry.from_dict(i, feed))]
