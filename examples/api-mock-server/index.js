const { ApolloServer, gql, ApolloError } = require('apollo-server');
const fetch = require('sync-fetch');

const typeDefs = gql`
    type Hello {
        message: String!,
        num: Int!,
    }
    type Query {
        hello: Hello!
    }
    type Mutation {
        newNumber: Int!
    }
`;

function sleep(time, callback) {
    var stop = new Date().getTime();
    while(new Date().getTime() < stop + time) {
        ;
    }
    callback();
}

let number = 1;

let authenticated = (args, token) => {
    let response = fetch("http://mockserver_gql_client_auth:8002/graphql", {
        method: "POST",
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            query: "{token}"
        })
    }).json();
    if (response.data.token === token) {
        return true;
    }
    return false;
}

const resolvers = {
    Query: {
        hello: async (parent, args, context) => {
            if (authenticated(args, context.token)) {
                return {token: args.token, num: number };
            }
            throw new ApolloError("unauthorized", 200, {"missing_token": true});
        },
    },
    Mutation: {
        newNumber: async (parent, args, context) => {
            if (authenticated(args, context.token)) {
                number = Math.round(Math.random() * 100);
                return number;
            }
            throw new ApolloError("unauthorized", 200, {"missing_token": true});
        }
    }
};

const server = new ApolloServer({
    typeDefs,
    resolvers,
    context: ({ req }) => {
        const token = req.headers.authorization || '';
        return {token};
    },
    mocks: true,
    mockEntireSchema: false,
});

server.listen({port: 8001}).then(({ url }) => {
    console.log(`🚀 Server ready at ${url}`)
});
