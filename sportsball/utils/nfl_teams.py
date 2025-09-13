"""NFL team information and utilities."""

from typing import Dict, List, Optional
from dataclasses import dataclass


@dataclass
class Team:
    """NFL team information."""
    name: str
    abbreviation: str
    city: str
    division: str
    conference: str


class NFLTeams:
    """NFL team data and utilities."""
    
    # Team data mapping
    TEAMS = {
        # AFC East
        "BUF": Team("Bills", "BUF", "Buffalo", "AFC East", "AFC"),
        "MIA": Team("Dolphins", "MIA", "Miami", "AFC East", "AFC"),
        "NE": Team("Patriots", "NE", "New England", "AFC East", "AFC"),
        "NYJ": Team("Jets", "NYJ", "New York", "AFC East", "AFC"),
        
        # AFC North
        "BAL": Team("Ravens", "BAL", "Baltimore", "AFC North", "AFC"),
        "CIN": Team("Bengals", "CIN", "Cincinnati", "AFC North", "AFC"),
        "CLE": Team("Browns", "CLE", "Cleveland", "AFC North", "AFC"),
        "PIT": Team("Steelers", "PIT", "Pittsburgh", "AFC North", "AFC"),
        
        # AFC South
        "HOU": Team("Texans", "HOU", "Houston", "AFC South", "AFC"),
        "IND": Team("Colts", "IND", "Indianapolis", "AFC South", "AFC"),
        "JAX": Team("Jaguars", "JAX", "Jacksonville", "AFC South", "AFC"),
        "TEN": Team("Titans", "TEN", "Tennessee", "AFC South", "AFC"),
        
        # AFC West
        "DEN": Team("Broncos", "DEN", "Denver", "AFC West", "AFC"),
        "KC": Team("Chiefs", "KC", "Kansas City", "AFC West", "AFC"),
        "LV": Team("Raiders", "LV", "Las Vegas", "AFC West", "AFC"),
        "LAC": Team("Chargers", "LAC", "Los Angeles", "AFC West", "AFC"),
        
        # NFC East
        "DAL": Team("Cowboys", "DAL", "Dallas", "NFC East", "NFC"),
        "NYG": Team("Giants", "NYG", "New York", "NFC East", "NFC"),
        "PHI": Team("Eagles", "PHI", "Philadelphia", "NFC East", "NFC"),
        "WAS": Team("Commanders", "WAS", "Washington", "NFC East", "NFC"),
        
        # NFC North
        "CHI": Team("Bears", "CHI", "Chicago", "NFC North", "NFC"),
        "DET": Team("Lions", "DET", "Detroit", "NFC North", "NFC"),
        "GB": Team("Packers", "GB", "Green Bay", "NFC North", "NFC"),
        "MIN": Team("Vikings", "MIN", "Minnesota", "NFC North", "NFC"),
        
        # NFC South
        "ATL": Team("Falcons", "ATL", "Atlanta", "NFC South", "NFC"),
        "CAR": Team("Panthers", "CAR", "Carolina", "NFC South", "NFC"),
        "NO": Team("Saints", "NO", "New Orleans", "NFC South", "NFC"),
        "TB": Team("Buccaneers", "TB", "Tampa Bay", "NFC South", "NFC"),
        
        # NFC West
        "ARI": Team("Cardinals", "ARI", "Arizona", "NFC West", "NFC"),
        "LAR": Team("Rams", "LAR", "Los Angeles", "NFC West", "NFC"),
        "SF": Team("49ers", "SF", "San Francisco", "NFC West", "NFC"),
        "SEA": Team("Seahawks", "SEA", "Seattle", "NFC West", "NFC"),
    }
    
    # Alternative name mappings for data scraping
    NAME_MAPPINGS = {
        # Common variations
        "New England Patriots": "NE",
        "New York Jets": "NYJ",
        "New York Giants": "NYG",
        "Los Angeles Chargers": "LAC",
        "Los Angeles Rams": "LAR",
        "Las Vegas Raiders": "LV",
        "Tampa Bay Buccaneers": "TB",
        "San Francisco 49ers": "SF",
        "Kansas City Chiefs": "KC",
        "Green Bay Packers": "GB",
        
        # City names
        "Buffalo": "BUF",
        "Miami": "MIA",
        "Baltimore": "BAL",
        "Cincinnati": "CIN",
        "Cleveland": "CLE",
        "Pittsburgh": "PIT",
        "Houston": "HOU",
        "Indianapolis": "IND",
        "Jacksonville": "JAX",
        "Tennessee": "TEN",
        "Denver": "DEN",
        "Dallas": "DAL",
        "Philadelphia": "PHI",
        "Washington": "WAS",
        "Chicago": "CHI",
        "Detroit": "DET",
        "Minnesota": "MIN",
        "Atlanta": "ATL",
        "Carolina": "CAR",
        "New Orleans": "NO",
        "Arizona": "ARI",
        "Seattle": "SEA",
    }
    
    @classmethod
    def get_team(cls, identifier: str) -> Optional[Team]:
        """Get team by abbreviation or name."""
        # Try direct abbreviation lookup
        if identifier in cls.TEAMS:
            return cls.TEAMS[identifier]
        
        # Try name mapping
        if identifier in cls.NAME_MAPPINGS:
            return cls.TEAMS[cls.NAME_MAPPINGS[identifier]]
        
        # Try fuzzy matching on team names
        identifier_lower = identifier.lower()
        for abbr, team in cls.TEAMS.items():
            if (identifier_lower in team.name.lower() or 
                identifier_lower in team.city.lower() or
                identifier_lower in f"{team.city} {team.name}".lower()):
                return team
        
        return None
    
    @classmethod
    def get_abbreviation(cls, identifier: str) -> Optional[str]:
        """Get team abbreviation from any identifier."""
        team = cls.get_team(identifier)
        return team.abbreviation if team else None
    
    @classmethod
    def get_all_teams(cls) -> List[Team]:
        """Get list of all teams."""
        return list(cls.TEAMS.values())
    
    @classmethod
    def get_division_teams(cls, division: str) -> List[Team]:
        """Get teams in a specific division."""
        return [team for team in cls.TEAMS.values() if team.division == division]
    
    @classmethod
    def get_conference_teams(cls, conference: str) -> List[Team]:
        """Get teams in a specific conference."""
        return [team for team in cls.TEAMS.values() if team.conference == conference]
