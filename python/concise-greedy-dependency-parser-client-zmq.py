#
# Request-reply client in Python
# Connects REQ socket to tcp://localhost:5560
# Sends "Hello" to server, expects "World" back
#

# Tried : "yum install zeromq-devel"    ## Happens to be 2.2 on Fedora 20
# Using : "yum install zeromq3-devel"   ## Happens to be 3.2.4 on Fedora 20
# Needs : "easy_install pyzmq"
import zmq

# Prepare our context and socket
context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5559")

print("Client is connected to socket on broker frontend")

# Do 10 requests, waiting each time for a response
for request in range(1,11):
    print("Sending %d" % request)
    socket.send(b"Hello")
    
    # Wait for response
    message = socket.recv()
    print("Received reply %s [%s]" % (request, message))