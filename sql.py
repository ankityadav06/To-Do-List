from flask import Flask, render_template, request
import mysql.connector
from mysql.connector import Error

app = Flask(__name__)

try:
    db = mysql.connector.connect(
        host='localhost',       # Replace with your database host
        user='root',            # Replace with your database user
        password='ankit5787@',  # Replace with your database password
        database='todolist'     # Replace with your database name
    )
except Error as e:
    print(f"Error connecting to MySQL Platform: {e}")

@app.route("/", methods=['GET', 'POST'])
def hello_world():
    if request.method == 'POST':
        title = request.form['title']
        desc = request.form['desc']

        if title.strip() == '' or desc.strip() == '':
            return render_template("index.html", error="Title and Description cannot be empty.")
        
        try:
            mycursor = db.cursor()
            sql = "INSERT INTO todo (title, `desc`) VALUES (%s, %s)"
            mycursor.execute(sql, (title, desc))
            db.commit()  # Commit the changes
            mycursor.close()
        except Error as e:
            print(f"Error: {e}")
    
    try:
        cursor = db.cursor(dictionary=True)
        cursor.execute("SELECT sno, title, `desc`, date_created FROM todo")
        todos = cursor.fetchall()
        cursor.close()
    except Error as e:
        print(f"Error: {e}")

    return render_template("index.html", todos=todos)


if __name__ == '__main__':
    app.run(debug=True)
