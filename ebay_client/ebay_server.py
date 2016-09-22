import daemon
import logging
import argparse

import zmq
from ebaysdk.trading import Connection as Trading
from ebaysdk.exception import ConnectionError


def main():

    # Initialize ZMQ socket
    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind('tcp://127.0.0.1:5555')

    domain = 'api.sandbox.ebay.com'
    api = Trading(
        domain=domain,
        config_file='../settings/ebay.yaml'
    )

    while True:
        try:
            # Receive requests from ZMQ clients.
            ident = socket.recv_string()
            logging.debug('Received Ebay item ID:' + str(ident))
        except KeyboardInterrupt:
            logging.warning('Shutting down with ^C signal')
            context.destroy()
            return

        try:
            item_response = api.execute('GetItem', {'ItemID': ident})
        except ConnectionError as e:
            logging.error(e)
            socket.send_json({'error': repr(e)})
            return

        item_response_dict = item_response.dict()
        ebay_item = item_response_dict['Item']
        dropp_item = {
            'name': ebay_item['Title'],
            'id': ebay_item['ItemID'],
            'url': ebay_item['ListingDetails']['ViewItemURL'],
            'quantity': ebay_item['Quantity'],
            'current_price': ebay_item['SellingStatus']['CurrentPrice'],
            'quantity_sold': ebay_item['SellingStatus']['QuantitySold'],
            'status': ebay_item['SellingStatus']['ListingStatus'],
        }

        socket.send_json(dropp_item)
        logging.debug('Replying object for Ebay item ID:' + str(ident))

if __name__ == '__main__':

    parser = argparse.ArgumentParser(
        description='The Ebay client.')

    parser.add_argument(
        '--daemon',
        action='store_true',
        help='Start the application as a Unix deamon')

    args = parser.parse_args()

    if args.daemon:
        with daemon.DaemonContext():
            main()
    else:
        main()
