const express = require('express');
const bodyParser = require('body-parser');
const fs = require('fs');
const path = require('path');
const app = express();
const port = 5000; // Change this port as needed

const ACCESS_KEY = 'your-access-key'; // Replace with your actual access key
const dbPath = path.join(__dirname, 'db.json');

app.use(bodyParser.json());

// Middleware to check for the access key in query parameters
function authenticate(req, res, next) {
  const accessKey = req.query.access_key;
  if (!accessKey) {
    return res.status(401).send({ message: 'Access key is missing' });
  }

  if (accessKey !== ACCESS_KEY) {
    return res.status(401).send({ message: 'Invalid access key' });
  }

  next();
}

// Read the existing courses from db.json
function readDb() {
  if (!fs.existsSync(dbPath)) {
    return { courses: [] };
  }
  const data = fs.readFileSync(dbPath, 'utf-8');
  return JSON.parse(data);
}

// Write the updated courses to db.json
function writeDb(data) {
  fs.writeFileSync(dbPath, JSON.stringify(data, null, 2));
}

app.post('/courses', authenticate, (req, res) => {
  const course = req.body;
  console.log('Course received:', course);

  // Read existing courses
  const db = readDb();

  // Add the new course
  db.courses.push(course);

  // Write the updated courses back to db.json
  writeDb(db);

  res.status(201).send({ message: 'Course created successfully', course });
});

// New GET endpoint to retrieve courses
app.get('/courses', authenticate, (req, res) => {
  const db = readDb();
  res.status(200).send(db.courses);
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
