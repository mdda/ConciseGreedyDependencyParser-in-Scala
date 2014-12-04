#
# Request-reply client in Python
# Connects REQ socket to tcp://localhost:5560
# Sends "Hello" to server, expects "World" back
#

# Needs : "yum install zeromq-devel"   ## Happens to be 2.2 on Fedora 20
# Needs : "easy_install pyzmq"
import zmq

# Prepare our context and sockets
context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5560")

print("Connected to socket")

# Do 10 requests, waiting each time for a response
for request in range(1,11):
    print("Sending %d" % request)
    socket.send(b"Hello")
    message = socket.recv()
    print("Received reply %s [%s]" % (request, message))
