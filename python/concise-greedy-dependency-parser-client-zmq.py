#
# Request-reply client in Python
# Connects REQ socket to tcp://localhost:5559 (expecting a broker's bind() there)
#

import json

# Tried : "yum install zeromq-devel"    ## Happens to be 2.2 on Fedora 20
# Using : "yum install zeromq3-devel"   ## Happens to be 3.2.4 on Fedora 20
# Needs : "easy_install pyzmq"
import zmq

# Prepare our context and socket
context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5559")

print("Client is connected to socket on broker frontend")

if False : # This is the Hello World test
  # Do 10 requests, waiting each time for a response
  for request in range(1,11):
      print("Sending %d" % request)
      socket.send(b"Hello")
      
      # Wait for response
      message = socket.recv()
      print("Received reply %d [%s]" % (request, message))

# Do 10 requests, waiting each time for a response
for num in range(1,11):
    body = {
      'sentences': [
        'Between 1994 and 2000, Ms Ding worked in the Shenzhen office of Yixing Silk-Linen Factory and was in charge of regional sales. ', 
      ],
    }
    #r = requests.post("http://127.0.0.1:5001/redcatlabs/handshakes/api/v1.0/parse", data=json.dumps(body))

    # See : http://augustl.com/blog/2013/zeromq_instead_of_http/
    request = dict(
      method = 'POST',
      path = 'redcatlabs/handshakes/api/v1.0/parse',
      body = body,
    )
    
    print("Sending Request # %d" % num)
    socket.send(json.dumps(request))
    
    # Wait for response
    message = socket.recv()
    print("Received reply # %d [%s]" % (num, message))
