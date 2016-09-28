import zmq
import pprint


context = zmq.Context()

socket = context.socket(zmq.REQ)
socket.connect('tcp://127.0.0.1:5555')

id_list = [
    '152097153823',
    '152110086748',
    '152086931137',
    '152022486781',
    # '110183593331',
    # '110183691021',
]

items = []
for ident in id_list:
    print('Sending request No %s' % ident)
    socket.send_string(ident)
    items.append(socket.recv_json())


with open('dump.json', 'w') as file:
    pp = pprint.PrettyPrinter()
    pp.pprint(items)
