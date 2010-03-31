# Be sure to restart your server when you modify this file.

# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.
ActionController::Base.session = {
  :key    => '_hashcloud_session',
  :secret => '4d02c32fd290e44db1939af0bbdc118d55ccdc8c721c49d940c7ab8001e8a87cc6920fd20bbaa2fece1990090bf423db225654365f803fa5993c2c259b2e701f'
}

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rake db:sessions:create")
# ActionController::Base.session_store = :active_record_store
