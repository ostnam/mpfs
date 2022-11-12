import os

import flask
import flask_login

import login
import datamanager

app = flask.Flask(__name__)
data_manager = datamanager.DataManager()

app.secret_key = os.environ["MPFS_SESSION_KEY"]

login_manager = flask_login.LoginManager()
login_manager.init_app(app)
login_manager.login_view = "login_page"

@login_manager.user_loader
def load_user(user_id):
    return login.User.get(user_id)

@app.route("/")
def index():
    return flask.redirect(flask.url_for("feeds"))

@app.route("/login", methods=["GET", "POST"])
def login_page():
    if flask.request.method == "GET":
        return flask.render_template(
            "login_page.html"
        )
    elif flask.request.method ==  "POST":
        submitted_token = flask.request.values.get("token")
        user = login.User.get(submitted_token)
        flask_login.login_user(user, remember=True)
        return flask.redirect(flask.url_for("feeds"))

@app.route("/feeds")
@flask_login.login_required
def feeds():
    feeds = data_manager.get_subscribed_feeds()
    entries = data_manager.get_entries(feeds)
    feeds_dict = {i.url: i for i in feeds}
    return flask.render_template(
        "feeds.html",
        entries=entries,
        feeds_dict=feeds_dict
    )

@app.route("/logout")
@flask_login.login_required
def logout():
    flask_login.logout_user()
    return "logged out"
