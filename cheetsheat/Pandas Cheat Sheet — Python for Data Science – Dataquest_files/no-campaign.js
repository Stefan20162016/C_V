/*! Thrive Ultimatum - 2019-02-27
* http://www.thrivethemes.com/
* Copyright (c) 2019 Thrive Themes */

if(!window.ThriveGlobal||!window.ThriveGlobal.$j){var __thrive_$oJ=window.$;window.ThriveGlobal={$j:jQuery.noConflict()},__thrive_$oJ&&(window.$=__thrive_$oJ)}!function(a){a(function(){var b={action:TVE_Ult_Data.conversion_events_action,post_id:TVE_Ult_Data.post_id};window.TVE_Dash&&!TVE_Dash.ajax_sent?a(document).on("tve-dash.load",function(c){TVE_Dash.add_load_item("tu_conversion_events",b,a.noop)}):a.ajax({url:TVE_Ult_Data.ajaxurl,type:"post",data:b})})}(ThriveGlobal.$j);