# Overview

## Backend
Stores:
* watches
* store checkins
* store notifications

Potential storage backends:
* redis
* Xsql
* acid-state

## Notifiers
* email
* webhook
* pushover?

## Frontends
* configurator
* web

# Structure
* Utils.Vigilance.Types
* Utils.Vigilance.Backends.Acid
* Utils.Vigilance.Backends.Redis
* Utils.Vigilance.Frontends.ConfigFile
* Utils.Vigilance.Frontends.Web
* Utils.Vigilance.Notifiers.Email
* Utils.Vigilance.Notifiers.Webhook
* Utils.Vigilance.Sweeper
  * sweep
