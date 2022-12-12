from __future__ import annotations

from dataclasses import dataclass
from typing import NamedTuple, Optional, Tuple
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
    def from_dict(d: feedparser.util.FeedParserDict,
                  feed: str) -> Optional[FeedEntry]:
        """
        Builds a FeedEntry from a FeedParserDict.
        """
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
        """
        Builds a FeedEntry from a tuple, as returned by an SQL query.
        """
        return FeedEntry(
            title=t[0],
            link= l if (l := t[1]) else None,
            published=datetime.datetime.fromisoformat(t[2]),
            seen=bool(t[3]),
            feed=t[4]
        )

    def to_sql(self) -> Tuple[str, str, str, int, str]:
        """
        Builds a tuple of the fields of a FeedEntry, as needed for SQL insertion.
        """
        return (self.title,
                self.link if self.link else "",
                self.published.isoformat(),
                int(self.seen),
                self.feed)

class Feed(NamedTuple):
    name: str
    url: str

    @staticmethod
    def from_sql(t: Tuple[str, str]) -> Feed:
        """
        Builds a Feed from a tuple, as returned by an SQL query.
        """
        return Feed(
            name=t[0],
            url=t[1],
        )

    def to_sql(self) -> Tuple[str, str]:
        """
        Builds a tuple of the fields of a Feed, as needed for SQL insertion.
        """
        return (
            self.name,
            self.url,
        )

def refresh_feed(feed: str) -> list[FeedEntry]:
    """
    Retrieves the items of an RSS feed.
    """
    return [entry
            for i in feedparser.parse(feed).entries
            if (entry := FeedEntry.from_dict(i, feed))]
