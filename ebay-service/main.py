from flask import Flask
from flask.views import MethodView


class Item(MethodView):
    def get(self):
        return 'Hello World!'


app = Flask(__name__)


app.add_url_rule(
    '/', view_func=Item.as_view('item')
)


if __name__ == '__main__':
    # This is used when running locally. Gunicorn is used to run the
    # application on Google App Engine. See entrypoint in app.yaml.
    app.run(host='127.0.0.1', port=9090, debug=True)
