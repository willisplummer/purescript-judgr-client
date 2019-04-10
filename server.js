/* eslint no-console: 0 */
const express = require('express');
const bodyParser = require('body-parser');
const cookieParser = require('cookie-parser');
const qs = require('qs');
const request = require('request');
const path = require('path');

const PORT = 8080;
const app = express();
app.use((req, res, next) => {
  res.header("Access-Control-Allow-Origin", "http://localhost:1234");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  res.header("Access-Control-Allow-Credentials", "true");
  next();
});
app.use(bodyParser.json({ type: ['application/vnd.api+json', 'application/json'] }));
app.use(cookieParser())
app.use(express.static(path.join(__dirname, 'build')));

app.options('*', (req, res) => res.send({ status: 200 }));
app.all('*', (req, res) => {
  const jar = request.jar();
  const r = request.defaults({});

  const url = 'http://localhost:3001';
  const jwtCookie = request.cookie(`JWT-Cookie=${req.cookies['JWT-Cookie']}`)
  jar.setCookie(jwtCookie, url)

  r({
    url: url + req.path,
    method: req.method,
    body: JSON.stringify(req.body),
    jar,
    headers: {
      'Content-Type': 'application/json',
    },
  }, (err, apiResponse) => {
    if (apiResponse.statusCode < 400) {
      const setCookie = apiResponse.headers['set-cookie'];
      const jwt = setCookie && setCookie[0].split('=')[1]
      res.cookie('JWT-Cookie', jwt);
    }
  
    if (err) {
      throw err;
    }
  
    try {
      res.status(apiResponse.statusCode).send(JSON.parse(apiResponse.body));
    } catch (e) {
      res.status(apiResponse.statusCode).send({ status: apiResponse.statusCode });
    }
  });
});

app.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`);
});
