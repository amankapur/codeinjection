import os
import sys
import time
import socket
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from watchdog.events import FileModifiedEvent

class CodeChangedEventHandler(FileSystemEventHandler):
    def __init__(self, address, port):
        self.clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.clientsocket.connect((address,port))

    def on_modified(self, event):
        file_name, file_extension = os.path.splitext(event.src_path)
        if file_extension != ".jac" or event.is_directory: return

        with open (event.src_path, "r") as myfile:
            data = myfile.read()
            data = data.replace('\n', ' ') + '\n'
            print (data)
            self.clientsocket.sendall(data)


if __name__ == "__main__":
    path = '.'
    address = sys.argv[1] if len(sys.argv) > 1 else "localhost:3041"
    address, port = address.split(":")
    port = int(port)

    event_handler = CodeChangedEventHandler(address,port)
    observer = Observer()
    observer.schedule(event_handler, path, recursive=True)
    observer.start()

    try:
        while True:
            r = event_handler.clientsocket.recv(4096)
            if r == "READING":
                print "READY"
            elif r == "STEP":
                print "STEP"
                event_handler.clientsocket.sendall("\n") # tells other end to continue evaluating
            elif r == "DONE":
                print "DONE"
            # time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
        event_handler.clientsocket.sendall("\f\n") # tells the other end to close
        event_handler.clientsocket.close()
    observer.join()