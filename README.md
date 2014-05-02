Cypress Frankenfeld iamcypress@gmail.com
Julian Ceipek julian.ceipek@gmail.com
Aman Kapur amankapur91@gmail.com

We present to you JAC, a runtime code injection language. 

Run the interpreter:

    sml
    CM.make "func.cm";
    NetShell.netshell 9000;

And in another shell, run the file-watcher:

    pip install -r requirements.txt
    python watcher.py localhost:9000 

Now create any file with the extension `.jac` in the same directory as `watcher.py`.
Any time you edit and save this file, the python script will detect that it has changed, and will send the new code
to the interpreter, which will detect and insert changes. The interpreter will automatically lex, parse, and evaluate any
code it receives from the watcher.



