from json import dump

from flask import Flask
from flask_restful import Resource, Api
from ebaysdk.trading import Connection as Trading


class Item(Resource):

    @staticmethod
    def _get_status(obj):
        """ Return if the item is indexed.

        The function search for the exsistence of the optional field
        ReasonHideFromSearch, if it is found it is assumed that the item is
        hidden and therefore not active.
        """
        if obj.get('ReasonHideFromSearch'):
            return 'Inactive'
        else:
            return 'Active'

    @staticmethod
    def _get_quantity(obj):
        """ Return the stock quantity of the item.

        This function normalises a quirky behaviour of the trading API
        returning amount of items left in stock.

        It is not known if this is a bug or simply Ebay is shite but if an
        item is available for sale the field `Quantity` returns the amount of
        items left in stock as one would expect. If the item however is hidden
        the field returns the number of items sold.
        """
        if obj.get('ReasonHideFromSearch'):
            return 0
        else:
            return obj.get('Quantity')

    def get(self, item_id):

        try:
            item_response = ebayApi.execute('GetItem', {'ItemID': item_id})
        except ConnectionError as e:
            return dump({'error': repr(e)})
        item_response_dict = item_response.dict()

        ebay_item = item_response_dict['Item']
        dropp_item = {
            'name': ebay_item['Title'],
            'id': ebay_item['ItemID'],
            'url': ebay_item['ListingDetails']['ViewItemURL'],
            'quantity': self._get_quantity(ebay_item),
            'current_price': ebay_item['SellingStatus']['CurrentPrice'],
            'quantity_sold': ebay_item['SellingStatus']['QuantitySold'],
            'status': self._get_status(ebay_item)
        }

        return dropp_item


domain = 'api.ebay.com'
ebay_config_file = './ebay.yaml'
ebayApi = Trading(
    domain=domain,
    config_file=ebay_config_file
)

app = Flask(__name__)
api = Api(app)
api.add_resource(Item, '/item/<string:item_id>')


if __name__ == '__main__':
    # This is used when running locally. Gunicorn is used to run the
    # application on Google App Engine. See entrypoint in app.yaml.
    app.run(host='127.0.0.1', port=9090, debug=True)
