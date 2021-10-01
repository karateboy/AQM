package models

import scalikejdbc._

object Ozone8Hr {
  def createTab(year: Int)(implicit session: DBSession = AutoSession) = {
    val tabName = SQLSyntax.createUnsafely(s"O3_8Hr_${year}")
    val pkName = SQLSyntax.createUnsafely(s"PK_O3_8Hr_${year}")
    sql"""CREATE TABLE [dbo].[${tabName}](
            [DP_NO] [varchar](6) NOT NULL,
            [M_DateTime] [datetime2](7) NOT NULL,
            [Value] [float] NOT NULL,
            [Status] [varchar](3) NOT NULL,
            CONSTRAINT [${pkName}] PRIMARY KEY CLUSTERED
            (
              [DP_NO] ASC,
              [M_DateTime] ASC
            )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
       ) ON [PRIMARY]""".execute().apply()
  }

  def hasOzone8hrTab(year: Int)(implicit session: DBSession = AutoSession): Boolean = {
    val list = {
      sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
         """.map { rs => rs.string(1) }.list().apply()
    }
    list.contains(s"O3_8Hr_${year}")
  }
}
