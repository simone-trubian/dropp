import zmq


context = zmq.Context()

socket = context.socket(zmq.REQ)
socket.connect('tcp://127.0.0.1:5555')

id_list = [
    '110183593331',
    '110183691021',
]

for ident in id_list:
    print('Sending request No %s' % ident)
    socket.send_string(ident)

    message = socket.recv_json()
    print(message)
