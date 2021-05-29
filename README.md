# sr-proto-frontend
Frontend for a prototype project:
 - front: Elm + [Bulma CSS](https://bulma.io) 
 - back: GraphQL-exposing java app (source can be found [here](https://github.com/FrankElvin/sr-proto-backend))

Main file: [AllReports](./src/AllReports.elm), single-page application with all reports from the database table.

### Building
To run the project the following steps are required:
 - build and run the backend app
 - build frontend page `elm make ./src/AllReports.elm --output ./output/main.js`
 - copy the content of the `output` folder to a webserver
 - check the web page
