import logging

import zmq
from ebaysdk.trading import Connection as Trading
from ebaysdk.exception import ConnectionError


def main():

    # Initialize ZMQ socket
    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind('tcp://127.0.0.1:5555')

    domain = 'api.sandbox.ebay.com'
    api = Trading(domain=domain)
    dropp_items = []

    # Receive requests from ZMQ clients.
    ident = socket.recv_string()
    logging.debug('Received Ebay item ID:' + str(ident))

    try:
        item_response = api.execute('GetItem', {'ItemID': ident})
    except ConnectionError as e:
        logging.error(e)
        socket.send_json({'error': repr(e)})
        return

    item_response_dict = item_response.dict()
    ebay_item = item_response_dict['Item']
    dropp_items.append({
        'name': ebay_item['Title'],
        'id': ebay_item['ItemID'],
        'url': ebay_item['ListingDetails']['ViewItemURL'],
        'quantity': ebay_item['Quantity'],
        'current_price': ebay_item['SellingStatus']['CurrentPrice'],
        'quantity_sold': ebay_item['SellingStatus']['QuantitySold'],
        'status': ebay_item['SellingStatus']['ListingStatus'],
    })

    logging.debug('Replying object for Ebay item ID:' + str(ident))
    socket.send_json(dropp_items)

if __name__ == '__main__':
    main()
