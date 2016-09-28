import daemon
import logging
import logging.config
import argparse
import yaml

import zmq
from ebaysdk.trading import Connection as Trading
from ebaysdk.exception import ConnectionError


def get_status(obj):
    """ Return if the item is indexed.

    The function search for the exsistence of the optional field
    ReasonHideFromSearch, if it is found it is assumed that the item is hidden
    and therefore not active.
    """
    if obj.get('ReasonHideFromSearch'):
        return 'Inactive'
    else:
        return 'Active'


def get_quantity(obj):
    """ Return the stock quantity of the item.

    This function normalises a quirky behaviour of the trading API returning
    amount of items left in stock.

    It is not known if this is a bug or simply Ebay is shite but if an item is
    available for sale the field `Quantity` returns the amount of items left in
    stock as one would expect. If the item however is hidden the field returns
    the number of items sold.
    """
    if obj.get('ReasonHideFromSearch'):
        return 0
    else:
        return obj.get('Quantity')


def main(settings, logger):

    domain = settings['domain']
    socket_address = settings['socket_address']
    ebay_config_file = settings['ebay_config_file']

    # Initialize ZMQ socket
    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(socket_address)

    api = Trading(
        domain=domain,
        config_file=ebay_config_file
    )

    while True:
        try:
            # Receive requests from ZMQ clients.
            ident = socket.recv_string()
            logger.debug('Received Ebay item ID:' + str(ident))
        except (SystemExit, KeyboardInterrupt) as e:
            logger.warning('Shutting down with %s signal' % repr(e))
            socket.close()
            context.destroy()
            return

        try:
            item_response = api.execute('GetItem', {'ItemID': ident})
        except ConnectionError as e:
            logger.error(e)
            socket.send_json({'error': repr(e)})
            return

        item_response_dict = item_response.dict()
        ebay_item = item_response_dict['Item']
        dropp_item = {
            'name': ebay_item['Title'],
            'id': ebay_item['ItemID'],
            'url': ebay_item['ListingDetails']['ViewItemURL'],
            'quantity': get_quantity(ebay_item),
            'current_price': ebay_item['SellingStatus']['CurrentPrice'],
            'quantity_sold': ebay_item['SellingStatus']['QuantitySold'],
            'status': get_status(ebay_item)
        }

        socket.send_json(dropp_item)
        logger.debug('Replying object for Ebay item ID:' + str(ident))

if __name__ == '__main__':

    parser = argparse.ArgumentParser(
        description='The Ebay client.')

    parser.add_argument(
        '--daemon',
        action='store_true',
        help='Start the application as a Unix deamon')

    args = parser.parse_args()

    # Configuration
    with open('../settings/settings.yaml') as f:
        settings = yaml.load(f)

    settings = settings['ebay_daemon']
    logging.config.dictConfig(settings['logger'])
    logger = logging.getLogger('dev')

    if args.daemon:
        with daemon.DaemonContext():
            main(settings, logger)
    else:
        main(settings, logger)
