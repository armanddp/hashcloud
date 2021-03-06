-record(user, {
	   lang,
       statuses_count,
       profile_link_color,
       location,
       following,
       profile_background_image_url,
       profile_image_url,
	   contributors_enabled,
       profile_sidebar_fill_color,
       profile_background_tile,
       description,
       screen_name,
       profile_sidebar_border_color,
       url,
       verified,
       geo_enabled,
       followers_count,
       friends_count,
       profile_background_color,
       protected,
       notifications,
       favourites_count,
       name,
       created_at,
       profile_text_color,
       id,
       time_zone,
       utc_offset
}).

-record(tweet, 
	{
		text,
		in_reply_to_screen_name,
		truncated,
		place,
		in_reply_to_status_id,
		source,
		coordinates,
		geo,
		favorited,
		in_reply_to_user_id,
		contributors,
		user = #user{},
		id,
		created_at
}).