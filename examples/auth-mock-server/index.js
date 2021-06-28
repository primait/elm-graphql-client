const { ApolloServer, gql, ApolloError } = require('apollo-server');
const {v4} = require('uuid');

let actual_token = v4();

const typeDefs = gql`
    type Query {
        token: String!
    }
    type Mutation {
        newToken: String!
    }
`;

const resolvers = {
    Query: {
        token: () => {
            return actual_token;
        },
    },
    Mutation: {
        newToken: () => {
            actual_token = v4();
            return actual_token;
        }
    }
};

const server = new ApolloServer({
    typeDefs,
    resolvers,
    mocks: true,
    mockEntireSchema: false
});

server.listen({port: 8002}).then(({ url }) => {
    console.log(`🚀 Server ready at ${url}`)
});
