module Types exposing (..)

import Time

type alias FeedData = {
  url: String,
  name: String
  }


type alias Entry = {
  title: String,
  link: String,
  published: Time.Posix,
  seen: Bool,
  feed: String
  }
