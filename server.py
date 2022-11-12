import flask

import feed
import datamanager

app = flask.Flask(__name__)
data_manager = datamanager.DataManager()

@app.route("/")
def index():
    return flask.redirect(flask.url_for("/feeds"))

@app.route("/feeds")
def feeds():
    feeds = data_manager.get_subscribed_feeds()
    entries = data_manager.get_entries(feeds)
    return flask.render_template(
        "feeds.html",
        entries=entries
    )

