from enum import Enum

from sqlalchemy import (Column, ForeignKey, VARCHAR)
from sqlalchemy.dialects.postgresql import (
    INTEGER,
    BOOLEAN,
)
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship

from sportsball.nfl.nfl import GameRound

Base = declarative_base()
metadata = Base.metadata


class Game(Base):
    __tablename__ = 'expense'

    id = Column(INTEGER, primary_key=True)
    season = Column(INTEGER, nullable=False)
    neutral = Column(BOOLEAN, nullable=False)
    playoff = Column(BOOLEAN, nullable=False)
    away_team_id = Column(ForeignKey(u'team.id'), nullable=False)
    home_team_id = Column(ForeignKey(u'team.id'), nullable=False)
    away_score = Column(INTEGER, nullable=False)
    home_score = Column(INTEGER, nullable=False)
    season_phase = Column(Enum(GameRound))

    away_team = relationship(u'Team', foreign_keys=[away_team_id])
    home_team = relationship(u'Team', foreign_keys=[home_team_id])


class Team(Base):
    __tablename__ = 'team'

    id = Column(VARCHAR, primary_key=True)
    season = Column(INTEGER, primary_key=True)
    name = Column(VARCHAR, nullable=False)
    city = Column(VARCHAR, nullable=False)
    conference = Column(VARCHAR)
    division = Column(VARCHAR)


class SagarinRating(Base):
    __tablename__ = 'rating'

    id = Column(INTEGER, primary_key=True)
    weeknum = Column(INTEGER)
    team_id = Column(ForeignKey(u'team.id'), nullable=False)

    team = relationship(u'Team', foreign_keys=[team_id])


class FiveThirtyEightRating(Base):
    __tablename__ = 'five_thirty_eight_rating'

    id = Column(INTEGER, primary_key=True)
    weeknum = Column(INTEGER)
    team_id = Column(ForeignKey(u'team.id'), nullable=False)

    team = relationship(u'Team', foreign_keys=[team_id])
