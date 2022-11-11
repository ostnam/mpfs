from flask import Flask, render_template

import feed

app = Flask(__name__)

@app.route("/")
def index():
    return render_template("feeds.html",
            entries=feed.refresh_feed("http://feeds.arstechnica.com/arstechnica/index"))
