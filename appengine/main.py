import functools
import webapp2
import logging
from google.appengine.api import urlfetch


class MineAvaHandler(webapp2.RequestHandler):
    def get(self):
        # [START urlfetch-rpc-callback]
        def handle_result(rpc):
            result = rpc.get_result()
            self.response.write(result.content)
            logging.info('Handling RPC in callback: result {}'.format(result))

        urls = ['http://www.google.com',
                'http://www.github.com',
                'http://www.travis-ci.org']
        rpcs = []
        for url in urls:
            rpc = urlfetch.create_rpc()
            rpc.callback = functools.partial(handle_result, rpc)
            urlfetch.make_fetch_call(rpc, url)
            rpcs.append(rpc)

        # ... do other things ...

        # Finish all RPCs, and let callbacks process the results.

        for rpc in rpcs:
            rpc.wait()

        logging.info('Done waiting for RPCs')
        # [END urlfetch-rpc-callback]       self.write("Mining stuff")


class MainHandler(webapp2.RequestHandler):
    def get(self):
        self.response.write("Hello, world")


app = webapp2.WSGIApplication([
    ('/', MainHandler),
    ('/mine', MineAvaHandler),
], debug=True)
