"""
get_player_tracking_data.py
---------------------------
Author: elishayer
Created: 9/21/2016
---------------------------
Port of Chris' R code to Python to convert STATS SportVU data
from XML format to CSV files
"""

import csv
import xml.dom.minidom as minidom
import os
import time

import Queue
from threading import Thread
import logging

class SportVUParser(object):
    """
    Class to parse SportVU data from XML to CSV
    """
    def __init__(self, data_path, season_folders):
        self.data_path = data_path
        self.season_folders = season_folders

        self.logger = logging.getLogger('SportVU')
        self.logger.setLevel(logging.INFO)

        fmt = '%(asctime)s %(thread)5d [%(levelname)-5.5s] [%(name)-20.20s]  %(message)s'
        formatter = logging.Formatter(fmt=fmt, datefmt='%Y-%m-%d %H:%M:%S')

        stream_handler = logging.StreamHandler()
        stream_handler.setLevel(logging.INFO)
        stream_handler.setFormatter(formatter)
        self.logger.addHandler(stream_handler)

        self.game_queue = Queue.Queue()

        self.outputs = {}


    def parse(self):
        """
        Iterate through the file system and parse all XML files
        """
        for season_folder in self.season_folders:
            season_path = os.path.join(self.data_path, season_folder)
            self._parse_season(season_path)

        for _ in range(8):
            thread = Thread(target=self._parse_game_wrapper)
            thread.daemon = True
            thread.start()

        self.game_queue.join()
        self.logger.info('done!')

    def _parse_season(self, season_path):
        """
        Parse a single season of files
        @param {str} season_path
        """
        for game_id in os.listdir(season_path):
            game_path = os.path.join(season_path, game_id)
            self.game_queue.put((game_path, game_id))


    def _parse_game_wrapper(self):
        while True:
            game_path, game_id = self.game_queue.get()
            self.logger.info('game_id: %s, remaining: %s', game_id, self.game_queue.qsize())
            self._parse_game(game_path, game_id)
            self.game_queue.task_done()


    def _parse_game(self, game_path, game_id):
        """
        Parse the single file contained at the given path
        @param {str} game_path
        @param {str} game_id
        """
        player_tracking_path = os.path.join(game_path, 'Player Tracking')

        if not os.path.isdir(player_tracking_path):
            self.logger.info('game_id %s has no tracking folder', game_id)
            return

        self.outputs[game_id] = []

        quarter_filenames = os.listdir(player_tracking_path)

        if len(quarter_filenames) == 0:
            self.logger.info('game_id %s has no tracking data files', game_id)
            return

        start = time.time()

        for quarter_filename in quarter_filenames:
            quarter_path = os.path.join(player_tracking_path, quarter_filename)
            parsed = minidom.parse(quarter_path)

            game_type = parsed.getElementsByTagName('game-type')[0].attributes['id'].value
            home_id = parsed.getElementsByTagName('home-team')[0].getElementsByTagName('team-code')[0].attributes['id'].value
            away_id = parsed.getElementsByTagName('visiting-team')[0].getElementsByTagName('team-code')[0].attributes['id'].value

            self._parse_quarter(parsed, game_id, home_id, away_id)

        output_filename = '_'.join([game_id, game_type, home_id, away_id]) + '.csv'
        output_path = os.path.join(self.data_path, 'parsed', output_filename)

        writer = csv.writer(open(output_path, 'wb'))
        writer.writerow(['quarter', 'game_clock', 'shot_clock', 'ball_x', 'ball_y', 'ball_z',
                         'h1_id', 'h1_x', 'h1_y',
                         'h2_id', 'h2_x', 'h2_y',
                         'h3_id', 'h3_x', 'h3_y',
                         'h4_id', 'h4_x', 'h4_y',
                         'h5_id', 'h5_x', 'h5_y',
                         'a1_id', 'a1_x', 'a1_y',
                         'a2_id', 'a2_x', 'a2_y',
                         'a3_id', 'a3_x', 'a3_y',
                         'a4_id', 'a4_x', 'a4_y',
                         'a5_id', 'a5_x', 'a5_y'])
        writer.writerows(self.outputs[game_id])

        self.logger.info('quarters: %s, rows: %s, time: %s', len(quarter_filenames), len(self.outputs[game_id]), time.time() - start)
        del self.outputs[game_id]


    def _parse_quarter(self, parsed, game_id, home_id, away_id):
        """
        Parse a single quarter, putting the values into self.outputs[game_id]
        @param {xml.dom.minidom.Document} parsed
        @param {str} game_id
        @param {str} home_id
        @param {str} away_id
        """
        quarter = parsed.getElementsByTagName('sequences')[0].attributes['period'].value

        moments = parsed.getElementsByTagName('moment')

        for moment in moments:
            game_clock = moment.attributes['game-clock'].value
            shot_clock = moment.attributes['shot-clock'].value

            locations = moment.attributes['locations'].value.split(';')

            home_players = []
            away_players = []
            ball = []

            for location in locations:
                split = location.split(',')
                team_id = split[0]
                player_id = split[1]
                x_loc = split[2]
                y_loc = split[3]
                z_loc = split[4]

                if team_id == home_id:
                    home_players.append([player_id, x_loc, y_loc])
                elif team_id == away_id:
                    away_players.append([player_id, x_loc, y_loc])
                elif team_id == '-1':
                    ball.append([x_loc, y_loc, z_loc])

            home_players = self._pad_and_concatenate(home_players, 5)
            away_players = self._pad_and_concatenate(away_players, 5)
            ball = self._pad_and_concatenate(ball, 1)

            self.outputs[game_id].append([quarter, game_clock, shot_clock] + ball + home_players + away_players)


    @staticmethod
    def _pad_and_concatenate(arr, target):
        """
        Pad a list to make the target length, the concatenate into a single list
        @param {list<list<str>>} arr
        @param {int} target
        @returns {list<str>}
        """
        if len(arr) < target:
            for _ in range(target - len(arr)):
                arr.append(['', '', ''])

        concatenated = []

        for sub_arr in arr:
            concatenated = concatenated + sub_arr

        return concatenated




def main(data_path, season_folders):
    """
    Create a ParseSportVU object, and use it to parse
    @param {str} data_path
    @param {list<str>} season_folders
    """
    SportVUParser(data_path, season_folders).parse()

if __name__ == '__main__':
    DATA_PATH = '/Users/elishayer/Dropbox/SportVU Data/'
    SEASON_FOLDERS = ['2015-2016']
    main(DATA_PATH, SEASON_FOLDERS)
